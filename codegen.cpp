#include "ast.h"
#include "codegen.h"

map<string, std::stack<pair<Value *, ast*>>> symbol;
BasicBlock* libfunc;
static int cnt = 0;
static code *cur_code;
StructType *StructTy_struct;
static LLVMContext context;
static std::unique_ptr<Module> module = llvm::make_unique<Module>("my cool scheMe", context);;
static llvm::IRBuilder<> builder(context);

GlobalVariable * make_lib(string name, int num){

    std::vector<llvm::Type *> putsArgs;
    putsArgs.push_back(builder.getInt128Ty()->getPointerTo());
    putsArgs.push_back(builder.getInt128Ty()->getPointerTo());
    llvm::ArrayRef<llvm::Type*> argsRef(putsArgs); 

    llvm::FunctionType* fccType =
        llvm::FunctionType::get(
            llvm::Type::getInt128Ty(context),
            putsArgs, false
        );   

    Function *lib = (Function*) module->getOrInsertFunction(name,
        fccType
        );

  std::vector<llvm::Constant*> v2;
  v2.push_back(lib);
  v2.push_back(ConstantInt::get(builder.getInt64Ty(), num));


  llvm::ArrayRef<llvm::Constant*> argsRef2(v2);
  Constant* q = ConstantStruct::getAnon(argsRef2,true); 

  GlobalVariable *p = new GlobalVariable(
    *module,
    q->getType(),
    true,
    GlobalValue::ExternalLinkage,
    q,
    name+"_q");    

  v2.clear();
  v2.push_back(ConstantInt::get(builder.getInt64Ty(), SMRT_FUNCNOMEM));
  v2.push_back(p);
  llvm::ArrayRef<llvm::Constant*> consRef(v2);
  q = ConstantStruct::getAnon(consRef,true); 

  GlobalVariable *lib_t = new GlobalVariable(
    *module,
    q->getType(),
    true,
    GlobalValue::ExternalLinkage,
    q,
    name+"_t"); 

    return lib_t;    
}

void call_lib_functions(){
  BasicBlock* prev = builder.GetInsertBlock();

  llvm::FunctionType *funcType = llvm::FunctionType::get(builder.getVoidTy(), false);
  llvm::Function *call_lib = 
    llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "call_lib", module.get());

  libfunc = llvm::BasicBlock::Create(context, "libpoint", call_lib);
 
  builder.SetInsertPoint(libfunc);

  symbol["input"].push(make_pair(make_lib("smlib_input", 0),nullptr));
  symbol["string"].push(make_pair(make_lib("smlib_string", 1),nullptr));
  symbol["int"].push(make_pair(make_lib("smlib_int", 1),nullptr));
  symbol["float"].push(make_pair(make_lib("smlib_float", 1),nullptr));
  
  symbol["="].push(make_pair(make_lib("smlib_equal", 2),nullptr));
  symbol["<"].push(make_pair(make_lib("smlib_less", 2),nullptr));  
  symbol["+"].push(make_pair(make_lib("smlib_add", 2),nullptr));
  symbol["-"].push(make_pair(make_lib("smlib_sub", 2),nullptr));
  symbol["*"].push(make_pair(make_lib("smlib_mul", 2),nullptr));
  symbol["/"].push(make_pair(make_lib("smlib_div", 2),nullptr));
  
  symbol["and"].push(make_pair(make_lib("smlib_and", 2),nullptr));  
  symbol["or"].push(make_pair(make_lib("smlib_or", 2),nullptr));  
  symbol["not"].push(make_pair(make_lib("smlib_not", 1),nullptr));    
  
  symbol["length"].push(make_pair(make_lib("smlib_length", 1),nullptr));
  symbol["at"].push(make_pair(make_lib("smlib_at", 2),nullptr));
  
  symbol["cons"].push(make_pair(make_lib("smlib_cons", 2),nullptr));
  symbol["car"].push(make_pair(make_lib("smlib_car", 1),nullptr));
  symbol["cdr"].push(make_pair(make_lib("smlib_cdr", 1),nullptr));
  symbol["null"].push(make_pair(make_lib("smlib_null", 0),nullptr));
  
  symbol["int?"].push(make_pair(make_lib("smlib_isint", 1),nullptr));
  symbol["float?"].push(make_pair(make_lib("smlib_isfloat", 1),nullptr));
  symbol["pair?"].push(make_pair(make_lib("smlib_ispair", 1),nullptr));
  symbol["string?"].push(make_pair(make_lib("smlib_isstring", 1),nullptr));
  symbol["func?"].push(make_pair(make_lib("smlib_isfunc", 1),nullptr));

  builder.SetInsertPoint(prev);  

}


void codegen_init(){

  llvm::FunctionType *funcType = llvm::FunctionType::get(builder.getVoidTy(), false);
  llvm::Function *mainFunc = 
    llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "main", module.get());
 
  llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entrypoint", mainFunc);
 
  builder.SetInsertPoint(entry);


  call_lib_functions();
  llvm::FunctionType* make_lib_type =
        llvm::FunctionType::get(
            llvm::Type::getVoidTy(context),
             false
        );

  Function *lib = (Function*) module->getOrInsertFunction("call_lib",
        make_lib_type
        );
    
  builder.CreateCall(lib);  
}

void exit_gen(const char *file_name, Value* value){

    std::vector<llvm::Type *> putsArgs;
    putsArgs.push_back(builder.getInt128Ty());
    
    llvm::FunctionType* exit_type =
        llvm::FunctionType::get(
            builder.getVoidTy(),
            putsArgs, false
        );      


    Function *exit_g = (Function*) module->getOrInsertFunction("smrt_exit",
          exit_type
          );
      
    builder.CreateCall(exit_g, value);
    builder.CreateRetVoid();
    std::error_code ErrInfo;
    llvm::raw_ostream *out = new llvm::raw_fd_ostream(file_name, ErrInfo, llvm::sys::fs::F_None);
  builder.SetInsertPoint(libfunc);
  builder.CreateRetVoid();
  module->print(*out, nullptr);
  out->flush();
  //ee->freeMachineCodeForFunction(aveFunc);
  //delete ee;
  llvm_shutdown();

}
void codegen(ast *a, code *c, const char* file_name) {
  cur_code = c;
  codegen_init();
  exit_gen(file_name, a->codegen(nullptr));
}

Value* ast_int::codegen(ast* belong){

    Value* str =  builder.getInt64(val);

    Value *structVar = builder.CreateAlloca(builder.getInt64Ty(), builder.getInt64(128));

    Value *elementPtr = builder.CreateGEP(structVar,builder.getInt64(0));

    Value* fpcp = builder.CreateBitCast(str, Type::getInt64Ty(context));

    builder.CreateStore(builder.getInt64(SMRT_INT), elementPtr);

    elementPtr =  builder.CreateGEP(structVar,builder.getInt64(1));

    builder.CreateStore(fpcp, elementPtr);

   structVar = builder.CreateBitCast(structVar, builder.getInt128Ty()->getPointerTo());    

    Value* elementPt = builder.CreateLoad(builder.getInt128Ty(), structVar);

  return elementPt;

}

Value* ast_float::codegen(ast* belong){

    Value* str =  builder.getInt64(*(long long*)&val);

    Value *structVar = builder.CreateAlloca(builder.getInt64Ty(), builder.getInt64(128));

    Value *elementPtr = builder.CreateGEP(structVar,builder.getInt64(0));

  Value* fpcp = builder.CreateBitCast(str, Type::getInt64Ty(context));

    builder.CreateStore(builder.getInt64(SMRT_FLOAT), elementPtr);

    elementPtr =  builder.CreateGEP(structVar,builder.getInt64(1));

    builder.CreateStore(fpcp, elementPtr);

  structVar = builder.CreateBitCast(structVar, builder.getInt128Ty()->getPointerTo());    

    Value* elementPt = builder.CreateLoad(builder.getInt128Ty(), structVar);

    

  return elementPt;
}





Value* ast_string::codegen(ast* belong){


    Value* str = builder.CreateGlobalStringPtr(val.c_str());

    Value *structVar = builder.CreateAlloca(builder.getInt64Ty(), builder.getInt64(128));

    Value *elementPtr = builder.CreateGEP(structVar,builder.getInt64(0));

    Value* fpcp = builder.CreateBitCast(str, Type::getInt64Ty(context));

    builder.CreateStore(builder.getInt64(SMRT_STRING), elementPtr);

    elementPtr =  builder.CreateGEP(structVar,builder.getInt64(1));

    builder.CreateStore(fpcp, elementPtr);

    structVar = builder.CreateBitCast(structVar, builder.getInt128Ty()->getPointerTo());    

    Value* elementPt = builder.CreateLoad(builder.getInt128Ty(), structVar);

    symbol[val].push(make_pair(str, belong));

    BasicBlock* prev = builder.GetInsertBlock();



    builder.SetInsertPoint(libfunc);
    std::vector<llvm::Type *> putsArgs2;
    putsArgs2.push_back(builder.getInt8Ty()->getPointerTo());
  
    Value* fpcp2 = builder.CreateBitCast(str, Type::getInt8PtrTy(context));

    llvm::FunctionType* make_string_type =
        llvm::FunctionType::get(
            llvm::Type::getVoidTy(context),
            putsArgs2, false
        );

    Value * args[] = {fpcp2};

    Function *lambda = (Function*) module->getOrInsertFunction("smrt_reg_cosnt_string",
        make_string_type
        );
    builder.CreateCall(lambda, args);

    builder.SetInsertPoint(prev);
    return elementPt;
}

void pbreak(int i){
  printf("a point\n");
}

Value * ast_symbol::codegen(ast* belong){

  stack<pair<Value*, ast*>> &sbm_val = symbol[val];
  if(!sbm_val.empty()){

    ast* tmp = sbm_val.top().second;
    if(tmp == nullptr){

        return builder.CreateLoad(builder.CreateBitCast(sbm_val.top().first, Type::getInt128Ty(context)->getPointerTo()));
    }

    if(tmp != belong){

      map<string, pair<Value*, int>> &quot = *(static_cast<ast_list*>(belong)->env);
      int index;
      if(quot.find(val) == quot.end()){
        index = quot.size();
        quot[val] = make_pair((sbm_val.top()).first, index);
      }
      else
        index = quot[val].second;
      

      Function::arg_iterator AE = static_cast<ast_list*>(belong)->AI;
          
        Value *element = builder.CreateGEP(AE,builder.getInt64(index)); 
        Value *elemVal = builder.CreateLoad(element);       
  

      return elemVal;

    }
    else
      return (sbm_val.top()).first;
  }
  else{
    smc_error(string("error: ")
      + code::code_pos_str(pos)
      + string(" Unknown symbol ")
      + val
      + string("\n")
      + cur_code->get_preview(pos));
    return nullptr;
  }

}

Value* func_dfn(vector<ast*> val, ast* belong){

  ast_symbol * lft = static_cast<ast_symbol*>(val.at(1));
  ast_symbol * rgt = static_cast<ast_symbol*>(val.at(2));

  Value* upd = rgt->codegen(belong);
  symbol[lft->val].push(make_pair(upd, belong));

  Value* alloc = builder.getInt64(0);
  alloc = builder.CreateBitCast(alloc, Type::getInt128Ty(context));
  return alloc;
}


Value* ast_list::codegen(ast* belong){

  if(val.size() < 1){
        smc_error(string("error: ")
      + code::code_pos_str(pos)
      + string(" invalid expression ")
      + string("\n")
      + cur_code->get_preview(pos));
  }

  ast* tmp = val.at(0);

  if(tmp->get_type() == SYMBOL && static_cast<ast_symbol*>(tmp)->val == "define"){
      return func_dfn(val, belong);
    }
    else if(tmp->get_type() == SYMBOL && static_cast<ast_symbol*>(tmp)->val == "if")
      return (func_if(belong));   

    else if(tmp->get_type() == SYMBOL && static_cast<ast_symbol*>(tmp)->val == "lambda"){
      
      return (func_lambda(this));   
  } 
  else{
    
    Value* func = (val.at(0))->codegen(belong);
    Value *alloc = nullptr;

    if(val.size() > 1){

      alloc = builder.CreateAlloca(builder.getInt128Ty(), builder.getInt64(128 * (val.size()-1))); 

      for(int i = 1; i < val.size(); i++){
          Value *element = builder.CreateGEP(alloc,builder.getInt64(i-1)); 
          Value *elemVal = val[i]->codegen(belong);
          builder.CreateStore(elemVal,element);         
      } 
    }
    else{
      alloc = builder.getInt64(0);
    alloc = builder.CreateBitCast(alloc, Type::getInt128Ty(context)->getPointerTo());        
    }
      Value* fpcp = builder.CreateBitCast(func, Type::getInt128Ty(context));        

      Value* pass_arg[] = {fpcp, alloc, builder.getInt64(val.size() - 1)};

      std::vector<llvm::Type *> putsArgs;
      putsArgs.push_back(builder.getInt128Ty());
      putsArgs.push_back(builder.getInt128Ty()->getPointerTo());
      putsArgs.push_back(builder.getInt64Ty());
    
    llvm::FunctionType* apply_type =
        llvm::FunctionType::get(
            llvm::Type::getInt128Ty(context),
            putsArgs, false
        );      


      Function *apply = (Function*) module->getOrInsertFunction("smrt_apply",
          apply_type
          );
      
      Value* ret = builder.CreateCall(apply, pass_arg);
      
      return ret;     
  }
}

Value* func_dfn_dlt(vector<ast*> val){
  ast_symbol * lft = static_cast<ast_symbol*>(val.at(1));
  symbol[lft->val].pop();
  return NULL;
}

Value* ast_list::func_if(ast* belong){

    BasicBlock* entry = builder.GetInsertBlock();
    builder.SetInsertPoint(entry);

    Function *func = builder.GetInsertBlock()->getParent();
    Value* cond = val.at(1)->codegen(belong);

    if(val.size() != 4){
      smc_error(string("error: ")
        + code::code_pos_str(pos)
        + string(" Too many/few args for if statement\n")
        + cur_code->get_preview(pos));
    }

    std::vector<llvm::Type *> putsArgs;
    putsArgs.push_back(llvm::Type::getInt128Ty(context));

  llvm::FunctionType* make_true_type =
        llvm::FunctionType::get(
            llvm::Type::getInt32Ty(context),
            putsArgs, false
        );


    Function *is_true = (Function*) module->getOrInsertFunction("smrt_is_true",
        make_true_type
        );

    cond = builder.CreateCall(is_true, cond);
  cond = builder.CreateICmpNE(cond, ConstantInt::get(Type::getInt32Ty(context), 0));

  BasicBlock * then_bb = BasicBlock::Create(context, "then",func);
  BasicBlock * else_bb = BasicBlock::Create(context, "else");
  BasicBlock * merge_bb = BasicBlock::Create(context, "merge");

 
  builder.CreateCondBr(cond, then_bb, else_bb);
   builder.SetInsertPoint(then_bb);

    Value *thenval = val.at(2)->codegen(belong);
    if(!thenval){
      smc_error(string("error: ")
        + code::code_pos_str(pos)
        + string(" Invalid cond expr for if\n")
        + cur_code->get_preview(pos));
    }
    builder.CreateBr(merge_bb);
  then_bb = builder.GetInsertBlock();

    func->getBasicBlockList().push_back(else_bb);
  builder.SetInsertPoint(else_bb);

  Value *elseval;
  if(val.size() == 4)
    elseval = val[3]->codegen(belong);
  else{
    elseval = builder.getIntN(128,0);
    //return nullptr;
  }

  builder.CreateBr(merge_bb);
  else_bb = builder.GetInsertBlock(); 

    func->getBasicBlockList().push_back(merge_bb);
    builder.SetInsertPoint(merge_bb);

    PHINode * phi = builder.CreatePHI(builder.getInt128Ty(), 2, "if_func");

    phi->addIncoming(thenval, then_bb);
    phi->addIncoming(elseval, else_bb);

    return phi;

}

Value* ast_list::func_lambda(ast* belong){
    env = new map<string, pair<Value*,int>>();

    llvm::BasicBlock *entryP = builder.GetInsertBlock();

    std::vector<llvm::Type *> putsArgs;
    putsArgs.push_back(builder.getInt128Ty()->getPointerTo());
    putsArgs.push_back(builder.getInt128Ty()->getPointerTo());
    llvm::ArrayRef<llvm::Type*> argsRef(putsArgs); 

    llvm::FunctionType* fccType =
        llvm::FunctionType::get(
            builder.getInt128Ty(),
            putsArgs, false
        );

    char name_buf[20];
  memset(name_buf, 0, 10);
  sprintf(name_buf, "lambda_%d", cnt);

  cnt++;

    llvm::Function *fcc = 
      llvm::Function::Create(fccType, llvm::Function::InternalLinkage, name_buf, module.get());
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(context, "entrypoint", fcc);
    //builder.SetInsertPoint(entry);
    if(val.at(1)->get_type() != LIST){
      smc_error(string("error: ")
        + code::code_pos_str(pos)
        + string(" invalid args for lambda\n")
        + cur_code->get_preview(pos));
    }
  ast_list* arg = (ast_list*)val.at(1);

  //builder.SetInsertPoint(entryP); 

  /* process args of function */
  builder.SetInsertPoint(entry);


  AI = fcc->arg_begin();
  AI->setName("env");
  AI++;
  AI->setName("args");  

  for(int i = 0; i < (arg->val).size(); i++){
      if((arg->val)[i]->get_type() != SYMBOL){
              smc_error(string("error: ")
        + code::code_pos_str(pos)
        + string(" Invalid args for lambda\n")
        + cur_code->get_preview(pos));
      }

      Value *element = builder.CreateGEP(AI,builder.getInt64(i)); 
      Value *elemVal = builder.CreateLoad(element);  
        symbol[static_cast<ast_symbol*>((arg->val)[i])->val].push(make_pair(elemVal, belong));

  }


  AI = fcc->arg_begin();

    for(int i = 2; i < val.size() - 1; i++){
      ast* tmp = val.at(i);
      if(tmp->get_type() == LIST){
        if(static_cast<ast_list*>(tmp)->val[0]->get_type() == SYMBOL && static_cast<ast_symbol*>(static_cast<ast_list*>(tmp)->val[0])->val == "define"){
          func_dfn(static_cast<ast_list*>(tmp)->val, belong);
        }
      }
    }

    ast* tmp = val.at(val.size() - 1);
    Value* result = nullptr;

    if(tmp->get_type() == LIST){
      if(static_cast<ast_list*>(tmp)->val[0]->get_type() == SYMBOL && static_cast<ast_symbol*>(static_cast<ast_list*>(tmp)->val[0])->val == "define"){
          result = func_dfn(static_cast<ast_list*>(tmp)->val, belong);  
      }
      else
        result = tmp->codegen(belong);        
    }
    else{
      result = tmp->codegen(belong);
    }

    for(int i = 2; i < val.size(); i++){
      ast* tmp = val.at(i);

      if(tmp->get_type() == LIST && 
        static_cast<ast_list*>(tmp)->val[0]->get_type() == SYMBOL && static_cast<ast_symbol*>(static_cast<ast_list*>(tmp)->val[0])->val == "define"){
          func_dfn_dlt(static_cast<ast_list*>(tmp)->val);
        }     
    }  

  for(int i = 0; i < (arg->val).size(); i++){
      symbol[static_cast<ast_symbol*>((arg->val)[i])->val].pop();

  }



    builder.CreateRet(result);


    builder.SetInsertPoint(entryP);

    Value* alloc2 = nullptr;
    int env_size = (*env).size();

    if(env_size != 0){

      alloc2 = builder.CreateAlloca(builder.getInt128Ty(), builder.getInt64(128 * env_size));  
      for (const pair<string, pair<Value*, int>> p : *env) {

        Value *element = builder.CreateGEP(alloc2,builder.getInt64(p.second.second));
        builder.CreateStore(p.second.first,element);    

     }
   }
   else{
      alloc2 = builder.getIntN(128,0);
      alloc2 = builder.CreateBitCast(alloc2, Type::getInt128Ty(context)->getPointerTo());    

   }

     std::vector<llvm::Type *> putsArgs2;
     putsArgs2.push_back(builder.getInt8Ty()->getPointerTo());
     putsArgs2.push_back(builder.getInt64Ty());
     putsArgs2.push_back(builder.getInt128Ty()->getPointerTo());
     putsArgs2.push_back(builder.getInt64Ty());
  
     Value* fpcp = builder.CreateBitCast(fcc, Type::getInt8PtrTy(context));

  llvm::FunctionType* make_lambda_type =
        llvm::FunctionType::get(
            llvm::Type::getInt128Ty(context),
            putsArgs2, false
        );

    Value * args[] = {fpcp, builder.getInt64((arg->val).size()), alloc2,  builder.getInt64((*env).size())};

    Function *lambda = (Function*) module->getOrInsertFunction("smrt_make_lambda",
        make_lambda_type
        );

    Value* ret = builder.CreateCall(lambda, args);


    delete env;
    env = nullptr;
    return ret;

}
