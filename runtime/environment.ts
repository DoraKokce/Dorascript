import { Identifier, MemberExpr } from "../frontend/ast.ts";
import { evaluate } from "./interpreter.ts";
import { ObjectVal, ArrayVal, RuntimeVal,make_bool,make_native_func,make_null, make_number, make_obj, NumberVal, StringVal } from "./values.ts";
import { printValues } from "./eval/native-fns.ts";

export default class Environment {
    private parent?: Environment;
    private variables: Map<string,RuntimeVal>;
    private constants: Set<string>;
    constructor(parentENV?: Environment) {
        const global = parentENV ? true : false;
        this.parent = parentENV;
        this.variables = new Map();
        this.constants = new Set();
    }

    public declareVar (varname:string,value:RuntimeVal,constant:boolean):RuntimeVal {
        if (this.variables.has(varname)) {
            throw `Cannot declare variable ${varname}. As it already is defined.`;
        }

        this.variables.set(varname,value);
        
        if (constant){
            this.constants.add(varname);
        }
        return value;
    }
    
    public assignVar(varname:string,value:RuntimeVal):RuntimeVal {
        const env = this.resolve(varname);
        if (env.constants.has(varname)) {
            throw `Cannot reasign to variable ${varname} as it was declared constant.`;
        }        
        env.variables.set(varname,value);

        return value;
    }

    public lookupVar (varname:string):RuntimeVal {
        const env = this.resolve(varname);
        return env.variables.get(varname) as RuntimeVal;
    }

    public resolve(varname:string):Environment {
        if (this.variables.has(varname)) {
            return this;
        }

        if (this.parent == undefined) {
            throw `Cannot resolve '${varname}' as it does not exist.`;
        }

        return this.parent.resolve(varname);
    }

    public lookupOrMutObject(expr: MemberExpr, value?: RuntimeVal, property?: Identifier): RuntimeVal {
        let pastVal;
        if (expr.object.kind === 'MemberExpr') {
            pastVal = this.lookupOrMutObject(expr.object as MemberExpr, undefined, (expr.object as MemberExpr).property as Identifier);
        } else {
            const varname = (expr.object as Identifier).symbol;
            const env = this.resolve(varname);

            pastVal = env.variables.get(varname);
        }

        switch(pastVal.type) {
            case "object": {
                const currentProp = (expr.property as Identifier).symbol;
                const prop = property ? property.symbol : currentProp;
                if (value) (pastVal as ObjectVal).properties.set(prop, value);
                if (currentProp) pastVal = ((pastVal as ObjectVal).properties.get(currentProp) as ObjectVal);

                return pastVal;
            }
            case "array": {
                const numRT: RuntimeVal = evaluate(expr.property, this);
                if(numRT.type != "number") throw "Arrays do not have keys: " + expr.property;
                const num = (numRT as NumberVal).value;
                if(value) (pastVal as ArrayVal).values[num] = value;

                return (pastVal as ArrayVal).values[num];
            }
            default:
                throw "Cannot lookup or mutate type: " + pastVal.type;
        }
    }
}

export function createGlobalEnv() {
    const env = new Environment();
    env.declareVar("true",make_bool(true),true);
    env.declareVar("false",make_bool(false),true);
    env.declareVar("null",make_null(),true);

    env.declareVar("println",make_native_func((args) => {
        printValues(args);
        return make_null();
    }
    ),true)
    
    env.declareVar("Math", make_obj(
        new Map()
            .set("pi", make_number(Math.PI))
            .set("e", make_number(Math.E))
            .set("sqrt", make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                return make_number(Math.sqrt(arg));
            }))
            .set("random", make_native_func((args) => {
                const arg1 = (args[0] as NumberVal).value;
                const arg2 = (args[1] as NumberVal).value;

                const min = Math.ceil(arg1);
                const max = Math.floor(arg2);
                return make_number(Math.floor(Math.random() * (max - min + 1)) + min);
            }))
            .set("round", make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                return make_number(Math.round(arg));
            }))
            .set("ceil", make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                return make_number(Math.ceil(arg));
            }))
            .set("floor", make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                return make_number(Math.floor(arg));
            }))
            .set("abs", make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                return make_number(Math.abs(arg));
            }))
            .set("pow", make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                const arg2 = (args[1] as NumberVal).value;
                return make_number(Math.pow(arg,arg2));
            }))
            .set("cos",make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                return make_number(Math.cos(arg));
            }))
            .set("acos",make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                return make_number(Math.acos(arg));
            }))
            .set("asin",make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                return make_number(Math.asin(arg));
            }))
            .set("sin",make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                return make_number(Math.sin(arg));
            }))            
            .set("tan",make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                return make_number(Math.tan(arg));
            }))
            .set("atan",make_native_func((args) => {
                const arg = (args[0] as NumberVal).value;
                return make_number(Math.atan(arg));
            }))
    ), true)

    return env
}