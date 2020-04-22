Running GCL.fsx using fsharp (fsi.exe) will ask the user to input a string.
It is then checked whether the string matches the grammar of the subset of GCL given in the assignment description, and an AST is built for the string if it matches.
If the string doesn't match the GCL grammar, the user is given a hint as to which string and position the parser couldn't recognize.

FSLexYacc 10.0.0 is required to run the program and GCL.fsx must be in the same folder as the following files:


FsLexYacc.Runtime.dll 
GCLLexer.fs
GCLParser.fs
GCLTypesAST.fs


In addition to this, tests.fsx is included, running this file will run a few tests to see if different operators are recognized.

GCL.Fsx will compile two program graphs in the graphviz format and save these 
as NFA.gv and DFS.gv which are non-deterministic and deterministic Program Graphs respectively.

When running GCL.fsx and inputting an expression, if it is valid the user will be asked to input the variables to be initliazed.
After inputting initial values, the interpreter will run on the program graph made by the compiler, and output the values of the different variables when the program has finished running.
The output will also contain the end-node as well as whether or not the program terminated or got stuck.

Currently there is support for interpreting arrays and performing operations on arrays, however it is not possible to initiliaze arrays. This will be added in the future.