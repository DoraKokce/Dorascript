import { Identifier, MemberExpr } from "../frontend/ast.ts";
import { evaluate } from "./interpreter.ts";
import { ObjectVal, RuntimeVal,make_bool,make_native_func,make_null, make_number, make_obj } from "./values.ts";

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

    env.declareVar("console",make_obj(
        new Map()
            .set("log",make_native_func((args) => {
                console.log(...args);
                return make_null();
            }))

            .set("error",make_native_func((args) => {
                console.error(...args);
                throw args;
            }))

            .set("clear",make_native_func(() => {
                console.clear();
                return make_null();
            }))
    ),true)

    env.declareVar("Date",make_obj(
        new Map()
           .set("now",make_native_func(() => {
                return make_number(Date.now());
            }))
    ),true);
    return env
}