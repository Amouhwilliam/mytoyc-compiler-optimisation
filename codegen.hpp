/*
 * codegen.h
 *
 *  Created on: Mar 10, 2019
 *      Author: faber
 */

#ifndef CODEGEN_H_
#define CODEGEN_H_

#include <memory>
#include <stack>
#include <string>
#include <iostream>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Bitcode/BitstreamReader.h>
#include <llvm/Bitcode/BitstreamWriter.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/raw_ostream.h>

class CodeGenContext;
class Node;

/// Interface for classes that can generate code via llvm
class CodeGenerator {
public:
    virtual ~CodeGenerator(){};
    virtual llvm::Value* codeGen(CodeGenContext& context) const = 0;
};


/// Class CodeGenBlock combines symbol table information with
// current code generation blocks
class CodeGenBlock {
public:
    /// The current basic block
    llvm::BasicBlock *block;

    /// The local variables visible in the current basic block
    std::map<std::string, llvm::Value*> locals;

    CodeGenBlock(llvm::BasicBlock *blk,
            const std::map<std::string, llvm::Value*> &lc,
            llvm::Value *retVal=nullptr):
            block(blk),
            locals(lc){
    }

    CodeGenBlock(const CodeGenBlock&other){
        *this = other;
    }
    const CodeGenBlock &operator=(const CodeGenBlock&other){
        block=other.block;
        locals=other.locals;
        return *this;
    }

    ~CodeGenBlock(){};
};


class CodeGenContext {
    std::stack<CodeGenBlock> blocks;
    llvm::Function *mainFunction = nullptr;
    /// Map representing the builtin LLVM types for our primitive data types
    std::map<std::string, llvm::Type*> llvmTypeOf;

public:
    llvm::LLVMContext MyContext;
    /// Needed for NIfStatement: Actually, the recommended way of creating IR code is
    // via IRBuilder -- so let us use this approach, here...
    llvm::IRBuilder<> irBuilder;

    llvm::Module module;
    CodeGenContext() :
            MyContext(), irBuilder(MyContext), module("main", MyContext) {
    }
    virtual ~CodeGenContext(){}

    /// LLVM is better typed than mytoyc -- we'll need a way to express
    // a type for program generation -- but only for an int type
    llvm::Type* getIntType() {
        return llvm::Type::getInt32Ty(this->MyContext);
    }
    /* William AMOUH modification */
    llvm::Type * getVoidType(){
           return llvm::Type::getVoidTy(this->MyContext);
    }
    /* William AMOUH modification */
    llvm::Type * getType(int type){
            llvm::Type* t = nullptr;
            switch(type)
            {
                //  case TINT
                case 1:
                    t = llvm::Type::getInt32Ty(this->MyContext);
                    break;
                // case TVOID
                case 0:
                    t = llvm::Type::getVoidTy(this->MyContext);
                    break;
                default:
                    break;
            }
            return t;


        }
    llvm::GenericValue runCode();
    void generateCode(CodeGenerator& root);
    std::map<std::string, llvm::Value*>& currentLocals() { return blocks.top().locals; }
    llvm::BasicBlock *currentBlock() { return this->irBuilder.GetInsertBlock(); }

    /// create a new basic block within a code block,
    // inheriting local variables; if this is the first block created,
    // it is also the first block of our program, and does not inherit
    // any variables
    void pushBlock(llvm::BasicBlock *block)
    {
        if(blocks.empty()){
            blocks.push(CodeGenBlock(block,std::map<std::string, llvm::Value*>()));
        }else{
            blocks.push(CodeGenBlock(block,blocks.top().locals));
        }
        irBuilder.SetInsertPoint(block);
    }

    /// create a new basic block that is appended to the current basic block
    // (which is removed from the stack -- only the last basic block within
    // the hierarchy of basic blocks is used on the stack; no code is generated
    // "in the middle" of the program);
    // local variables are inherited from the current block
    void appendBlock(llvm::BasicBlock *block)
    {
        if(blocks.empty()){
            blocks.push(CodeGenBlock(block,std::map<std::string, llvm::Value*>()));
        }else{
            blocks.top().block = block;
        }
        this->irBuilder.SetInsertPoint(block);
    }

    CodeGenBlock &popBlock() {
        CodeGenBlock &top = blocks.top();
        blocks.pop();
         if(!blocks.empty()){
            this->irBuilder.SetInsertPoint(blocks.top().block);
        }
        return top;
    }
    /// get the info for a given [variable] name; die immediately, if the name
    // does not exist!
    llvm::Value* &lookup(std::string name){
        if(this->currentLocals().end()==this->currentLocals().find(name)) {
            std::cerr<<std::string("Undeclared variable ") + name<<std::endl;
        }
        return this->currentLocals()[name];
    }
    /// return the LLVM value pointer for the current location
    // of a given [variable] name; die immediately, if the name
    // does not exist!
    llvm::Value *getValue(std::string name){
        return lookup(name);
    }
    /// set the LLVM value pointer for a given variable name; die immediately, if the name
    // does not exist!
    void setValue(std::string name,llvm::Value*value){
        this->currentLocals()[name] = value;
    }
};

#endif /* CODEGEN_H_ */
