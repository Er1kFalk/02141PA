README

BEFORE RUNNING:

Change relevants paths to correct location. These include:
- The path to fslex.exe in run.bat
- The path to fsyacc.exe in run.bat
- The path to FsLexYacc.Runtime.dll in Task1.fsx

HOW TO RUN:
- Run the run.bat file

Type in the desired code in GCL. Two things are printed out:
- The first is the AST, as we create it in F# (with the token names from Task1TypesAST.fs). E.g. "x:=3" would show as Assign(x, Num(3))
- The second output is translating the AST back to GCL, in order to check that it was created correctly (to show that we can get from AST back to GCL)

Enjoy breaking the code