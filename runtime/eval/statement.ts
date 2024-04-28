import { Program, VarDeclaration, FunctionDeclaration } from "../../frontend/ast.ts";
import Environment from "../environment.ts";
import { evaluate } from "../interpreter.ts";
import { FunctionValue, RuntimeVal, make_null } from "../values.ts";

export function evaulate_program(program:Program,env:Environment):RuntimeVal {
    let lastEvaluated: RuntimeVal = make_null();

    for (const statement of program.body) {
        lastEvaluated = evaluate(statement,env);
    }
    return lastEvaluated
}

export function evaluate_var_declaration(declaration:VarDeclaration,env:Environment): RuntimeVal {
    const value = declaration.value ? evaluate(declaration.value,env) : make_null();
    return env.declareVar(declaration.identifier,value,declaration.constant);
}

export function evaluate_function_declaration(declaration:FunctionDeclaration,env:Environment): RuntimeVal {
    const fn = {
        type: "function",
        name: declaration.name,
        paremeters:declaration.parameters,
        declarationEnv:env,
        body: declaration.body     
    } as FunctionValue;

    return env.declareVar(declaration.name,fn,true);
}