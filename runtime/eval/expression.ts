import { AssignmentExpr, BinaryExpr, CallExpr, Identifier, ObjectLiteral } from "../../frontend/ast.ts";
import Environment from "../environment.ts";
import { evaluate } from "../interpreter.ts";
import { FunctionValue, NativeFnValue, NumberVal, ObjectVal, RuntimeVal, make_null } from "../values.ts";

function evaluate_numeric_binary_expr(lhs:NumberVal,rhs:NumberVal,op:string): NumberVal {
    let res:number;
    if (op == "+") {
        res = lhs.value + rhs.value;
    }
    else if (op == "-") {
        res = lhs.value - rhs.value;
    }
    else if (op == "*") {
        res = lhs.value * rhs.value;
    }
    else if (op == "/") {
        res = lhs.value / rhs.value;
    }
    else {
        res = lhs.value % rhs.value;
    }

    return { type:"number", value:res } as NumberVal;
}

export function evauluate_bin_expr(binop:BinaryExpr,env:Environment): RuntimeVal {
    const lhs = evaluate(binop.left,env);
    const rhs = evaluate(binop.right,env);

    if (lhs.type == "number" && rhs.type == "number") {
        return evaluate_numeric_binary_expr(lhs as NumberVal, rhs as NumberVal,binop.operator);
    }

    return make_null();
}

export function evaluate_identifier(ident:Identifier,env:Environment): RuntimeVal {
    const val = env.lookupVar(ident.symbol);
    return val
}

export function evaluate_assignment(node: AssignmentExpr,env:Environment):RuntimeVal {
    if (node.assigne.kind !== "Identifier") {
        throw `Invalid LHS inaide assignment expr ${JSON.stringify(node.assigne)}`;
    }
    const varname = (node.assigne as Identifier).symbol;
    return env.assignVar(varname,evaluate(node.value,env));
}

export function evaluate_object_expr(obj:ObjectLiteral,env:Environment):RuntimeVal {
    const object = { type: "object",properties: new Map()} as ObjectVal;
    for (const { key,value } of obj.properties) {
        const runtimeVal = (value == undefined) ? env.lookupVar(key) : evaluate(value,env);

        object.properties.set(key,runtimeVal);
    }

    return object
}

export function evaluate_call_expr(expr:CallExpr,env:Environment):RuntimeVal {
    const args = expr.args.map((arg) => evaluate(arg,env));
    const fn = evaluate(expr.caller,env);

    if (fn.type == "native-fn") {
        const result = (fn as NativeFnValue).call(args,env);
        return result;
    } 
    
    if (fn.type == "function") {
        const func = fn as FunctionValue;
        const scope = new Environment(func.declarationEnv);

        for (let i = 0; i < func.paremeters.length; i++) {
            const varname = func.paremeters[i];
            scope.declareVar(varname,args[i],false);
        }

        let result: RuntimeVal = make_null();
        for (const stmt of func.body) {
            result = evaluate(stmt,scope);
        }
        
        return result;
    }   
    
    throw "Can't call value that is not a function: " + JSON.stringify(fn);
}
