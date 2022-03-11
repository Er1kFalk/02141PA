C:\Users\Erik\.nuget\packages\fslexyacc\10.0.0\build\fslex\net46\fslex.exe Task1Lexer.fsl --unicode
pause
C:\Users\Erik\.nuget\packages\fslexyacc\10.0.0\build\fsyacc\net46\fsyacc.exe Task1Parser.fsp --module Task1Parser
pause
dotnet fsi Task1.fsx
pause
