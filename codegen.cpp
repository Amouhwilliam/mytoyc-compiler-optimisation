/*
 * codegen.cpp
 *
 *  Created on: Mar 10, 2019
 *      Author: faber
 */

#include "codegen.hpp"
#include "node.hpp"
#include "parser.hpp"

#include <iostream>

/* Compile the AST into a module */
void CodeGenContext::generateCode(CodeGenerator& root) {
    std::cerr << "Generating code...\n";

    /* Create the top level interpreter function to call as entry */
    std::vector<llvm::Type*> argTypes;
    llvm::FunctionType *ftype = llvm::FunctionType::get(
            this->getIntType(),
            makeArrayRef(argTypes), false);
    mainFunction = llvm::Function::Create(ftype,
            llvm::GlobalValue::ExternalLinkage, "main", &module);
    llvm::BasicBlock *bblock = llvm::BasicBlock::Create(this->MyContext,
            "entry", mainFunction, 0);

    /* Push a new variable/block context */
    pushBlock(bblock);
    root.codeGen(*this); /* emit bytecode for the toplevel block */
    llvm::ReturnInst::Create(this->MyContext, currentBlock());
    popBlock();

    /* Print the bytecode in a human-readable format
     to see if our program compiled properly
     */
    std::cerr << "Code is generated.\n";

    llvm::legacy::PassManager pm;
    pm.add(llvm::createPrintModulePass(llvm::outs()));
    pm.run(module);
}

/* Executes the AST by running the main function */
llvm::GenericValue CodeGenContext::runCode() {
    std::cerr << "Running code...\n";
    llvm::ExecutionEngine *ee = llvm::EngineBuilder( std::unique_ptr<llvm::Module>(&module) ).create();
    ee->finalizeObject();
    std::vector<llvm::GenericValue> noargs;
    llvm::GenericValue v = ee->runFunction(mainFunction, noargs);
    std::cerr << "Code was run.\n";
    return v;
}

llvm::Value* Node::codeGen(CodeGenContext& context) const {
    std::cerr << "Node::codeGen -- should never get called! " << std::endl;
    return nullptr;
}

llvm::Value* NExpression::codeGen(CodeGenContext& context) const {
    std::cerr << "NExpression::codeGen -- should never get called! "
            << std::endl;
    return nullptr;
}

llvm::Value* NStatement::codeGen(CodeGenContext& context) const {
    return nullptr;
}

llvm::Value* NInteger::codeGen(CodeGenContext& context) const {
    std::cerr << "Creating integer: " << value << std::endl;
    return llvm::ConstantInt::get(context.getIntType(),
            value, true);
}



llvm::Value* NIdentifier::codeGen(CodeGenContext& context) const {
    std::cerr << "Creating identifier reference: " << name << std::endl;
    return new llvm::LoadInst(context.getValue(name), "", false,
            context.currentBlock());
}

llvm::Value* NFunctionCall::codeGen(CodeGenContext &context) const {
    llvm::Function *function = context.module.getFunction(id->name.c_str());
    if (function == NULL) {
        std::cerr << "no such function " << id->name << std::endl;
    }
    std::vector<llvm::Value*> args;
    NExpressionList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        args.push_back((*it)->codeGen(context));
    }
    llvm::CallInst *call = llvm::CallInst::Create(function, llvm::makeArrayRef(args), "", context.currentBlock());
    std::cerr << "Creating method call: " << id->name << std::endl;
    return call;
}

llvm::Value* NBinaryOperator::codeGen(CodeGenContext& context) const {
    std::cerr << "Creating comparison operation " << op << std::endl;
    llvm::Instruction::BinaryOps instr;
    switch (op) {
    case PLUS:
        instr = llvm::Instruction::Add;
        break;
    case MINUS:
        instr = llvm::Instruction::Sub;
        break;
    case MUL:
        instr = llvm::Instruction::Mul;
        break;
    case DIV:
        instr = llvm::Instruction::SDiv;
        break;
    default:
        return nullptr;
        break;
    }
    return llvm::BinaryOperator::Create(instr, lhs->codeGen(context),
            rhs->codeGen(context), "", context.currentBlock());
}

llvm::Value* NComparisonOperator::codeGen(CodeGenContext &context) const {
    std::cerr << "Creating comparison operation " << op << std::endl;
    llvm::CmpInst::Predicate pred;
    switch (op) {
    case EQ:
        pred = llvm::CmpInst::ICMP_EQ;
        break;
    case NE:
        pred = llvm::CmpInst::ICMP_NE;
        break;
    case LT:
        pred = llvm::CmpInst::ICMP_SLT;
        break;
    case GT:
        pred = llvm::CmpInst::ICMP_SGT;
        break;
    case LE:
        pred = llvm::CmpInst::ICMP_SLE;
        break;
    case GE:
        pred = llvm::CmpInst::ICMP_SGE;
        break;
    default:
        return nullptr;
        break;
    }
    // create comparison operation via IRBuilder in currentBlock
    return context.irBuilder.CreateICmp(pred, lhs->codeGen(context),
            rhs->codeGen(context), "" /*name for result variable*/);
}

llvm::Value* NAssignment::codeGen(CodeGenContext& context) const {
    std::cerr << "Creating assignment for " << lhs->name << std::endl;
    llvm::Value *rhsValue = rhs->codeGen(context);
     new llvm::StoreInst(rhsValue, context.getValue(lhs->name), false, context.currentBlock());
     return rhsValue;
}

llvm::Value* NBlock::codeGen(CodeGenContext& context) const {
    NStatementList::const_iterator it;
    llvm::Value *last = nullptr;
    std::cerr << "Creating block" << std::endl;
    for (it = statements.begin(); it != statements.end(); it++) {
        std::cerr << "Generating code for " << typeid(**it).name() << std::endl;
        last = (*it)->codeGen(context);
        // if the last statement did not return a value, it has to be
        // a return statement -- and after those, no further statements
        // make any sense (and producing them is dangerous w/ LLVM)
        if(nullptr==last)
            break;
    }
    return last;
}

llvm::Value* NExpressionStatement::codeGen(
        CodeGenContext& context) const {
    std::cerr << "Generating code for " << typeid(expression).name() << std::endl;
    return expression->codeGen(context);
}

llvm::Value* NReturnStatement::codeGen(CodeGenContext& context) const {
    std::cerr << "Generating return code for " << typeid(expression).name() << std::endl;
    if(nullptr != expression)
        {
            /* William AMOUH modification */
            // in case of a void return statement, we call create without return value argument
            llvm::Value *returnValue = expression->codeGen(context);
            llvm::ReturnInst::Create(context.MyContext, returnValue, context.currentBlock());
            std::cerr << "Return statement with expression" << std::endl;
        }
        else
        {
        	llvm::ReturnInst::Create(context.MyContext, context.currentBlock());
            std::cerr << "Return statement without expression" << std::endl;
        }
    // after a return statement, no other statements should be produced;
    // return nullptr to make caller aware of that fact
    return nullptr;
}

llvm::Value* NVariableDeclaration::codeGen(
        CodeGenContext& context) const {
    std::cerr << "Creating variable declaration " << id->name << std::endl;
    llvm::AllocaInst *alloc = new llvm::AllocaInst(
            context.getIntType(),
            0, id->name.c_str(), context.currentBlock());
    context.setValue(id->name, alloc);
    if (assignmentExpr != NULL) {
        NAssignment assn(id, assignmentExpr);
        assn.codeGen(context);
    }
    return alloc;
}

llvm::Value* NExternDeclaration::codeGen(CodeGenContext& context) const {
    std::vector<llvm::Type*> argTypes;
    NVariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(context.getIntType());
    }
    llvm::FunctionType *ftype = llvm::FunctionType::get(
    		/* William AMOUH modification*/
            context.getType(type), //getType instead of getIntType to cover the void type
            llvm::makeArrayRef(argTypes), false);
    llvm::Function *function = llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, id->name, &context.module);
    return function;
}

llvm::Value* NFunctionDeclaration::codeGen(CodeGenContext& context) const {
    std::vector<llvm::Type*> argTypes;
    NVariableList::const_iterator it;
    std::cerr << "Creating function: " << id->name << std::endl;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(context.getIntType());
    }
    llvm::FunctionType *ftype = llvm::FunctionType::get(
    		/* William AMOUH modification*/
            context.getType(type), //getType instead of getIntType to cover the void type
            llvm::makeArrayRef(argTypes), false);
    llvm::Function *function = llvm::Function::Create(ftype, llvm::GlobalValue::InternalLinkage, id->name.c_str(), &context.module);
    llvm::BasicBlock *bblock = llvm::BasicBlock::Create(context.MyContext, "entry", function, 0);

    context.pushBlock(bblock);

    llvm::Function::arg_iterator argsValues = function->arg_begin();
    llvm::Value* argumentValue;

    for (it = arguments.begin(); it != arguments.end(); it++) {
        argumentValue = (**it).codeGen(context);

        argumentValue = &*argsValues++;
        argumentValue->setName((*it)->id->name.c_str());
        llvm::StoreInst *inst = new llvm::StoreInst(argumentValue, context.currentLocals()[(*it)->id->name], false, context.currentBlock());
    }

    this->block->codeGen(context);
    context.popBlock();
    return function;
}

llvm::Value* NIfStatement::codeGen(CodeGenContext& context) const {
    NStatementList::const_iterator it;
    llvm::Value *cond = nullptr;
    llvm::Value *thenVal,*elseVal;
    bool needCombine = true;
    std::cerr << "Creating if-clause..." << std::endl;

    // evaluate condition
    cond = this->condition->codeGen(context);
    if (nullptr == cond) {
        std::cerr<<"NO COND"<<std::endl;
        return nullptr;
    }

    llvm::Function *theFunction = context.currentBlock()->getParent();
    // Create blocks for the then and else cases.  Insert the 'then' block at the
    // end of the function.

    // create BBs for then branch, else branch, endif (merge),
    // and insert the then branch right at the current end of the current function
    // (end: nullptr)
    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context.MyContext, "then", theFunction, nullptr);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context.MyContext, "else");
    llvm::BasicBlock *endifBB;

    // create a branch to either then or else branch
    context.irBuilder.CreateCondBr(cond,thenBB,elseBB);

    // create an unconditional branch to the then BB at the end of the current BB

    // start creating code for then branch -- in our case, local variables stay the same!
    // (variables are shared across the whole function!)
    context.appendBlock(thenBB);
    thenVal = this->ifBranch->codeGen(context);

    // we only need an endif basic block, if there is a
    // nontrivial then block
    needCombine = (nullptr != thenVal);

    // create an unconditional branch to the endif BB at the end
    if(needCombine){
        endifBB = llvm::BasicBlock::Create(context.MyContext, "endif");
        context.irBuilder.CreateBr(endifBB);
    }
    // stop creating code for then branch

    std::cerr << "...else... " << std::endl;

    if(nullptr!=this->elseBranch){
        // start creating code for else branch -- in our case, local variables stay the same!
        // (variables are shared across the whole function!)
        // thenBB had been added to theFunction automatically, elseBB needs to be added
        // manually, now
        theFunction->getBasicBlockList().push_back(elseBB);
        context.appendBlock(elseBB);
        elseVal = this->elseBranch->codeGen(context);
        // create an unconditional branch to the endif BB at the end
        if(nullptr!=thenVal)
            context.irBuilder.CreateBr(endifBB);
    }
    // stop creating code for else branch

    if(needCombine){
        // thenBB had been added to theFunction automatically, elseBB needs to be added
        // manually, now
        theFunction->getBasicBlockList().push_back(endifBB);
        context.appendBlock(endifBB); // locals needed here -- or reset block
    }
    return cond;
}

