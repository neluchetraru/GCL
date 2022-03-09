README file

# A parser for GCL

Key Files
GCLParser.fsp - parser file
GCLLexer.fsl - lexer
printer.fsx - script for testing our compiler (prettifier)

Installing and Running the Project
All the necessary libraries (FSLex & FSYacc) are included.
Run the script file “printer.fsx” using FSI.

Input and Output
To run the program we provide input by specifying the function “compile x”. The next x inputs must be programs desired to be converted in AST format.

Example:

> compile 9;;  
> Now we write any Extended Guarded Command Code. Also note it should be written in one line.
> y:=1; do x>0 -> y:=x\*y; x:=x-1 od //Factorial function

This will print out the AST for the factorial function:
Compose(ASSIGN(y,Num(1)),Do(ExecuteIf(GTExpr(VAR(x),Num(0)),Compose(ASSIGN(y,TimesExpr(VAR(x),VAR(y))),ASSIGN(x,MinusExpr(VAR(x),Num(1)))))))

In case of an erroneous program, the script will give an error: “Compilation Error”
