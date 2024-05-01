import { RuntimeVal, NumberVal, make_null } from "./values.ts"
import { AssignmentExpr, BinaryExpr, CallExpr, FunctionDeclaration, Identifier, MemberExpr, NumericLiteral, ObjectLiteral, Program, Stmt, VarDeclaration } from "../frontend/ast.ts"
import Environment from "./environment.ts";
import { evaluate_assignment, evaluate_call_expr, evaluate_identifier, evaluate_member_expr, evaluate_object_expr, evauluate_bin_expr } from "./eval/expression.ts";
import { evaulate_program, evaluate_var_declaration, evaluate_function_declaration } from "./eval/statement.ts";

export function evaluate(astNode: Stmt,env:Environment): RuntimeVal {
    switch (astNode.kind) {
        case "NumericLiteral":
            return { value: (astNode as NumericLiteral).value, type:"number"} as NumberVal
        case "Identifier":
            return evaluate_identifier(astNode as Identifier,env);
        case "ObjectLiteral":
            return evaluate_object_expr(astNode as ObjectLiteral,env);
        case "CallExpr":
            return evaluate_call_expr(astNode as CallExpr,env);
        case "AssignmentExpr":
            return evaluate_assignment(astNode as AssignmentExpr,env);
        case "BinaryExpr":
            return evauluate_bin_expr(astNode as BinaryExpr,env);
        case "Program":
            return evaulate_program(astNode as Program,env);
        case "VarDeclaration":
            return evaluate_var_declaration(astNode as VarDeclaration,env);
        case "FunctionDeclaration":
            return evaluate_function_declaration(astNode as FunctionDeclaration,env);
        case "MemberExpr":
            return evaluate_member_expr(env,undefined,astNode as MemberExpr);
        default:
            console.error("This AST Node has not yet been setup for interpretation.",astNode);
            Deno.exit(1);       
    }
}