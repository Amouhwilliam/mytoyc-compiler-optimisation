#include <iostream>
#include <cstdlib>

// has to be included first, so that PNode is available
#include "node.hpp"
#include "parser.hpp"
#include "tokens.hpp"

int main(int argc,char**argv) {
    // if a file name is supplied, open that file and
    // use it for yyin (parser input)
    if (argc > 1) {
        if(nullptr==(yyin = fopen(argv[1], "r"))){
            std::cerr<<std::string("Could not open file ")+argv[1]<<std::endl;
        }
    }
    yyparse();
    CodeGenContext context;
    context.generateCode(programBlock);
    context.runCode();

    // write out IR code
    if (argc > 2){
        std::string outFile = argv[1];
        FILE*llout = fopen(argv[2],"w");
        llvm::raw_fd_ostream out(fileno(llout),true);
        context.module.print(out, nullptr);
    }
    return 0;
}

