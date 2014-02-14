module Backend.OpenCL.Host where

import Data.Graph.Inductive
import Core.CoreGraph
import Backend.OpenCL.BuiltIn.Types
import Data.Graph.Analysis.Algorithms.Directed
import Data.List
import Backend.OpenCL.Types
import Backend.OpenCL.Function
import Backend.OpenCL.Literal
import Backend.OpenCL.Param
import Backend.OpenCL.Tools
import Tools
import Data.Maybe (fromJust)

edgePortName :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> LNode CalcEntity -> String -> String -> String
--edgePortName gr from to sFrom sTo = calcEntityName gr from ++ "_" ++ sFrom ++ "_" ++ calcEntityName gr to ++ "_" ++ sTo
edgePortName gr from to sFrom sTo = calcEntityName gr from

kernelArgIndex :: Gr CalcEntity EdgeRole -> LNode CalcEntity -> String -> Int
kernelArgIndex gr ln arg = fromJust $ findIndex (\ (a,_) -> a == arg) $ calcEntityPort gr ln

genHostHeader :: Gr CalcEntity EdgeRole -> (String, String)
genHostHeader gr = ("header.h", "")

genHostBody :: Gr CalcEntity EdgeRole -> (String, String)
genHostBody gr = ("HaskellCL.cpp", unlines [
        "#include <CL/cl.h>",
        "#include <string.h>",
        "#include <stdio.h>",
        "#include <stdlib.h>",
        "#include <iostream>",
        "#include <string>",
        "#include <fstream>",
        "using namespace std;",
        "",
        "/* convert the kernel file into a string */",
        "int convertToString(const char *filename, std::string& s)",
        "{",
        "       size_t size;",
        "       char*  str;",
        "       std::fstream f(filename, (std::fstream::in | std::fstream::binary));",
        "",
        "       if(f.is_open())",
        "       {",
        "               size_t fileSize;",
        "               f.seekg(0, std::fstream::end);",
        "               size = fileSize = (size_t)f.tellg();",
        "               f.seekg(0, std::fstream::beg);",
        "               str = new char[size+1];",
        "               if(!str)",
        "               {",
        "                       f.close();",
        "                       return 0;",
        "               }",
        "",
        "               f.read(str, fileSize);",
        "               f.close();",
        "               str[size] = '\\0';",
        "               s = str;",
        "               delete[] str;",
        "               return 0;",
        "       }",
        "       cout<<\"Error: failed to open file\\n:\"<<filename<<endl;",
        "       return -1;",
        "}",
        "int main(int argc, char* argv[])",
        "{",
        "       /*Step1: Getting platforms and choose an available one.*/",
        "       cl_uint numPlatforms;//the NO. of platforms",
        "       cl_platform_id platform = NULL;//the chosen platform",
        "       cl_int  status = clGetPlatformIDs(0, NULL, &numPlatforms);",
        "       if (status != CL_SUCCESS)",
        "       {",
        "               cout<<\"Error: Getting platforms!\"<<endl;",
        "               return 1;",
        "       }",
        "",
        "       /*For clarity, choose the first available platform. */",
        "       if(numPlatforms > 0)",
        "       {",
        "               cl_platform_id* platforms = (cl_platform_id* )malloc(numPlatforms* sizeof(cl_platform_id));",
        "               status = clGetPlatformIDs(numPlatforms, platforms, NULL);",
        "               platform = platforms[0];",
        "               free(platforms);",
        "       }",
        "",
        "       /*Step 2:Query the platform and choose the first GPU device if has one.Otherwise use the CPU as device.*/",
        "       cl_uint numDevices = 0;",
        "       cl_device_id *devices;",
        "       status = clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 0, NULL, &numDevices);",    
        "       if (numDevices == 0) //no GPU available.",
        "       {",
        "               cout << \"No GPU device available.\"<<endl;",
        "               cout << \"Choose CPU as default device.\"<<endl;",
        "               status = clGetDeviceIDs(platform, CL_DEVICE_TYPE_CPU, 0, NULL, &numDevices);",    
        "               devices = (cl_device_id*)malloc(numDevices * sizeof(cl_device_id));",
        "               status = clGetDeviceIDs(platform, CL_DEVICE_TYPE_CPU, numDevices, devices, NULL);",
        "       }",
        "       else",
        "       {",
        "               devices = (cl_device_id*)malloc(numDevices * sizeof(cl_device_id));",
        "               status = clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, numDevices, devices, NULL);",
        "       }",
        "",
        "       /*Step 3: Create context.*/",
        "       cl_context context = clCreateContext(NULL,1, devices,NULL,NULL,NULL);",
        "",
        "       /*Step 4: Creating command queue associate with the context.*/",
        "       cl_command_queue commandQueue = clCreateCommandQueue(context, devices[0], 0, NULL);",
        "",
        "       /*Step 5: Create program object */",
        "       const char *filename = \"kernels.cl\";",
        "       string sourceStr;",
        "       status = convertToString(filename, sourceStr);",
        "       const char *source = sourceStr.c_str();",
        "       size_t sourceSize[] = {strlen(source)};",
        "       cl_program program = clCreateProgramWithSource(context, 1, &source, sourceSize, NULL);",
        "",
        "       /*Step 6: Build program. */",
        "       status=clBuildProgram(program, 1,devices,NULL,NULL,NULL);",
        "       /*Step 7: Initial input,output for the host and create memory objects for the kernel*/",
        concatMap (\ln -> let
                                tp = sType $ calcEntityTypeIface gr ln
                                nm = calcEntityName gr ln
                          in unlines [
                "\t" ++ tp ++ " " ++ nm ++ ";",
                "\tcout << \"Input " ++ nm ++ ":\";\n\tcin >> " ++ nm ++ ";",
                "\tcl_mem " ++ nm ++ "InBuffer = clCreateBuffer(context, CL_MEM_READ_ONLY|CL_MEM_COPY_HOST_PTR, sizeof(" ++ tp ++ "),(void *) &" ++ nm ++ ", NULL);"
                ]) params,
        "\t" ++ busType ++ " out;\n",
        --unlines $ concatMap (\ (from, to, pm) -> map (\(f, t, tp) -> "\tcl_mem " ++ edgePortName gr from to f t ++ "Buffer  = clCreateBuffer(context, CL_MEM_WRITE_ONLY , sizeof(" ++ tp ++ "), NULL, NULL);") pm) nodeMap,
        --"\tcl_mem outputBuffer = clCreateBuffer(context, CL_MEM_WRITE_ONLY , sizeof(" ++ busType ++ "), NULL, NULL);\n",
        concatMap (\ln -> let
                                tp = sType $ calcEntityTypeIface gr ln
                                nm = calcEntityName gr ln
                          in "\tcl_mem " ++ nm ++ "Buffer = clCreateBuffer(context, CL_MEM_WRITE_ONLY , sizeof(" ++ tp ++ "), NULL, NULL);\n") $ labNodes gr,
        "       /*Step 8: Create kernel object */",
        concatMap (\ln -> let nm = calcEntityName gr ln in "\tcl_kernel " ++ nm ++ "Kernel = clCreateKernel(program,\"" ++ nm ++ "\", NULL);\n") $ labNodes gr,
        "       /*Step 9: Sets Kernel arguments.*/",
        concatMap (\ln -> let nm = calcEntityName gr ln in "\tstatus = clSetKernelArg(" ++ nm ++ "Kernel, 1, sizeof(cl_mem), (void *)&" ++ nm ++ "InBuffer);\n") params,
        unlines $ concatMap (\(from, to, pm) -> map (\(f, t, _) ->
                "\tstatus = clSetKernelArg(" ++ (calcEntityName gr from) ++ "Kernel, " ++ (show $ kernelArgIndex gr from f) ++ ", sizeof(cl_mem), (void *)&" ++ edgePortName gr from to f t ++ "Buffer);\n" ++
                "\tstatus = clSetKernelArg(" ++ (calcEntityName gr to) ++ "Kernel, " ++ (show $ kernelArgIndex gr to t) ++ ", sizeof(cl_mem), (void *)&" ++ edgePortName gr from to f t ++ "Buffer);"
                ) pm) nodeMap,
        "\tstatus = clSetKernelArg(" ++ (calcEntityName gr top) ++ "Kernel, 0, sizeof(cl_mem), (void *)&" ++ (calcEntityName gr top) ++ "Buffer);",
        "       /*Step 10: Running the kernel.*/",
        "\tsize_t global_work_size[1] = {1};",
        concatMap (\ln -> "\tcl_event " ++ calcEntityName gr ln ++ "Event;\n") $ labNodes gr,
        concatMap (\ln@(node,_) -> let nm = calcEntityName gr ln in if null $ lpre gr node then
                "\tstatus = clEnqueueNDRangeKernel(commandQueue, " ++ nm ++ "Kernel, 1, NULL, global_work_size, NULL, 0, NULL, &" ++ nm ++ "Event);\n"
                else unlines [
                "\tcl_event " ++ nm ++ "EventList[] = {" ++ (intercalate "," $ map (\(pn,_) -> (calcEntityName gr (pn,fromJust $ lab gr pn)) ++ "Event") $ lpre gr node) ++ "};",
                "\tstatus = clEnqueueNDRangeKernel(commandQueue, " ++ nm ++ "Kernel, 1, NULL, global_work_size, NULL, " ++ (show $ length $ lpre gr node) ++ ", " ++ nm ++ "EventList, &" ++ nm ++ "Event);"
                ]) $ map (\n -> (n,fromJust $ lab gr n)) $ reverse $ bfs (fst top) $ grev gr,
        "       /*Step 11: Read the cout put back to host memory.*/",
        "\tstatus = clEnqueueReadBuffer(commandQueue, " ++ calcEntityName gr top ++ "Buffer, CL_TRUE, 0, sizeof(" ++ busType ++ "), &out, 1, &" ++ calcEntityName gr top ++ "Event, NULL);",
        "\tcout<<out<<endl;",
        "",
        "       /*Step 12: Clean the resources.*/",
        concatMap (\ln -> "\tstatus = clReleaseKernel(" ++ calcEntityName gr ln ++ "Kernel);//Release kernel.\n") $ labNodes gr,
        "\tstatus = clReleaseProgram(program);     //Release the program object.",
        concatMap (\ln -> "\tstatus = clReleaseMemObject(" ++ calcEntityName gr ln ++ "InBuffer);//Release mem object.\n") params,
        concatMap (\ln -> "\tstatus = clReleaseMemObject(" ++ calcEntityName gr ln ++ "Buffer);//Release mem object.\n") $ labNodes gr,
        "\tstatus = clReleaseCommandQueue(commandQueue);//Release  Command queue.",
        "\tstatus = clReleaseContext(context);//Release context.",
        "",
        "       if (devices != NULL)",
        "       {",
        "               free(devices);",
        "               devices = NULL;",
        "       }",
        "       return 0;",
        "}"
        ]) where
                busType = (sType.calcEntityTypeIface gr.head.leavesOf) gr
                params = filter (isParamLN gr) $ labNodes gr
                top = head $ leavesOf gr
                nodeMap = map (getEdgePortMap gr) $ labEdges gr
                internals = (labNodes gr) \\ (top:params)