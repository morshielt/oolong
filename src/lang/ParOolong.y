-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParOolong where
import AbsOolong
import LexOolong
import ErrM

}

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%name pProgram Program
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&' { PT _ (TS _ 4) }
  '&&' { PT _ (TS _ 5) }
  '(' { PT _ (TS _ 6) }
  ')' { PT _ (TS _ 7) }
  '*' { PT _ (TS _ 8) }
  '+' { PT _ (TS _ 9) }
  '++' { PT _ (TS _ 10) }
  ',' { PT _ (TS _ 11) }
  '-' { PT _ (TS _ 12) }
  '--' { PT _ (TS _ 13) }
  '->' { PT _ (TS _ 14) }
  '/' { PT _ (TS _ 15) }
  ':' { PT _ (TS _ 16) }
  ';' { PT _ (TS _ 17) }
  '<' { PT _ (TS _ 18) }
  '<=' { PT _ (TS _ 19) }
  '=' { PT _ (TS _ 20) }
  '==' { PT _ (TS _ 21) }
  '>' { PT _ (TS _ 22) }
  '>=' { PT _ (TS _ 23) }
  'bool' { PT _ (TS _ 24) }
  'break' { PT _ (TS _ 25) }
  'continue' { PT _ (TS _ 26) }
  'else' { PT _ (TS _ 27) }
  'false' { PT _ (TS _ 28) }
  'if' { PT _ (TS _ 29) }
  'int' { PT _ (TS _ 30) }
  'print' { PT _ (TS _ 31) }
  'return' { PT _ (TS _ 32) }
  'string' { PT _ (TS _ 33) }
  'true' { PT _ (TS _ 34) }
  'void' { PT _ (TS _ 35) }
  'while' { PT _ (TS _ 36) }
  '{' { PT _ (TS _ 37) }
  '||' { PT _ (TS _ 38) }
  '}' { PT _ (TS _ 39) }

  L_ident {PT _ (TV $$)}
  L_integ {PT _ (TI $$)}
  L_quoted {PT _ (TL $$)}

%%

Ident :: {
  Ident 
}
: L_ident {
  Ident $1 
}

Integer :: {
  Integer 
}
: L_integ {
  read $1 
}

String :: {
  String 
}
: L_quoted {
  $1 
}

Program :: {
  Program 
}
: ListStmt {
  AbsOolong.Program (reverse $1)
}

Block :: {
  Block 
}
: '{' ListStmt '}' {
  AbsOolong.Block (reverse $2)
}

ListStmt :: {
  [Stmt]
}
: {
  [] 
}
| ListStmt Stmt {
  flip (:) $1 $2 
}

Stmt :: {
  Stmt 
}
: ';' {
  AbsOolong.Empty 
}
| Block {
  AbsOolong.BStmt $1 
}
| Type ListItem ';' {
  AbsOolong.Decl $1 $2 
}
| Ident '=' Expr ';' {
  AbsOolong.Ass $1 $3 
}
| Ident '++' ';' {
  AbsOolong.Incr $1 
}
| Ident '--' ';' {
  AbsOolong.Decr $1 
}
| 'return' Expr ';' {
  AbsOolong.Ret $2 
}
| 'return' ';' {
  AbsOolong.VRet 
}
| 'if' '(' Expr ')' Stmt {
  AbsOolong.Cond $3 $5 
}
| 'if' '(' Expr ')' Stmt 'else' Stmt {
  AbsOolong.CondElse $3 $5 $7 
}
| 'while' '(' Expr ')' Stmt {
  AbsOolong.While $3 $5 
}
| 'break' ';' {
  AbsOolong.Break 
}
| 'continue' ';' {
  AbsOolong.Continue 
}
| 'print' '(' Expr ')' ';' {
  AbsOolong.SPrint $3 
}
| Expr ';' {
  AbsOolong.SExp $1 
}
| Type Ident '(' ListArg ')' Block {
  AbsOolong.FnDef $1 $2 $4 $6 
}

Item :: {
  Item 
}
: Ident {
  AbsOolong.DefaultInit $1 
}
| Ident '=' Expr {
  AbsOolong.Init $1 $3 
}

ListItem :: {
  [Item]
}
: Item {
  (:[]) $1 
}
| Item ',' ListItem {
  (:) $1 $3 
}

Arg :: {
  Arg 
}
: Type Ident {
  AbsOolong.Arg $1 $2 
}
| Type '&' Ident {
  AbsOolong.RefArg $1 $3 
}

ListArg :: {
  [Arg]
}
: {
  [] 
}
| Arg {
  (:[]) $1 
}
| Arg ',' ListArg {
  (:) $1 $3 
}

Type :: {
  Type 
}
: 'int' {
  AbsOolong.Int 
}
| 'string' {
  AbsOolong.Str 
}
| 'bool' {
  AbsOolong.Bool 
}
| 'void' {
  AbsOolong.Void 
}
| '<' '(' ListByValOrRef ')' ':' Type '>' {
  AbsOolong.Fun $3 $6 
}

ByValOrRef :: {
  ByValOrRef 
}
: Type {
  AbsOolong.ByVal $1 
}
| Type '&' {
  AbsOolong.ByRef $1 
}

ListByValOrRef :: {
  [ByValOrRef]
}
: {
  [] 
}
| ByValOrRef {
  (:[]) $1 
}
| ByValOrRef ',' ListByValOrRef {
  (:) $1 $3 
}

Expr6 :: {
  Expr 
}
: '(' ListArg ')' ':' Type '->' Block {
  AbsOolong.ELambda $2 $5 $7 
}
| Ident {
  AbsOolong.EVar $1 
}
| Integer {
  AbsOolong.ELitInt $1 
}
| 'true' {
  AbsOolong.ELitTrue 
}
| 'false' {
  AbsOolong.ELitFalse 
}
| Ident '(' ListExpr ')' {
  AbsOolong.EApp $1 $3 
}
| String {
  AbsOolong.EString $1 
}
| '(' Expr ')' {
  $2 
}

Expr5 :: {
  Expr 
}
: '-' Expr6 {
  AbsOolong.Neg $2 
}
| '!' Expr6 {
  AbsOolong.Not $2 
}
| Expr6 {
  $1 
}

Expr4 :: {
  Expr 
}
: Expr4 MulOp Expr5 {
  AbsOolong.EMul $1 $2 $3 
}
| Expr5 {
  $1 
}

Expr3 :: {
  Expr 
}
: Expr3 AddOp Expr4 {
  AbsOolong.EAdd $1 $2 $3 
}
| Expr4 {
  $1 
}

Expr2 :: {
  Expr 
}
: Expr2 RelOp Expr3 {
  AbsOolong.ERel $1 $2 $3 
}
| Expr3 {
  $1 
}

Expr1 :: {
  Expr 
}
: Expr2 '&&' Expr1 {
  AbsOolong.EAnd $1 $3 
}
| Expr2 {
  $1 
}

Expr :: {
  Expr 
}
: Expr1 '||' Expr {
  AbsOolong.EOr $1 $3 
}
| Expr1 {
  $1 
}

ListExpr :: {
  [Expr]
}
: {
  [] 
}
| Expr {
  (:[]) $1 
}
| Expr ',' ListExpr {
  (:) $1 $3 
}

AddOp :: {
  AddOp 
}
: '+' {
  AbsOolong.Plus 
}
| '-' {
  AbsOolong.Minus 
}

MulOp :: {
  MulOp 
}
: '*' {
  AbsOolong.Times 
}
| '/' {
  AbsOolong.Div 
}
| '%' {
  AbsOolong.Mod 
}

RelOp :: {
  RelOp 
}
: '<' {
  AbsOolong.LTH 
}
| '<=' {
  AbsOolong.LE 
}
| '>' {
  AbsOolong.GTH 
}
| '>=' {
  AbsOolong.GE 
}
| '==' {
  AbsOolong.EQU 
}
| '!=' {
  AbsOolong.NE 
}

{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens


}

