use std::collections::HashMap;

use crate::ast::{Fun, Stmt, Ident, Expr, If, Block, Else};

pub struct SymbolTableBuilder<'src> {
    src: &'src str,
    map: HashMap<Ident, Symbol>,
    scope: HashMap<&'src str, Symbol>,
    symbol_count: usize,
}

#[derive(Debug)]
pub struct SymbolTable {
    map: HashMap<Ident, Symbol>,
    symbol_count: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Symbol(usize);

#[derive(Debug)]
pub struct SymbolMap<T> {
    map: Vec<T>,
}

impl<'src> SymbolTableBuilder<'src> {
    pub fn resolve(fun: &Fun, src: &str) -> SymbolTable {
        let mut names = SymbolTableBuilder {
            src,
            map: HashMap::new(),
            scope: HashMap::new(),
            symbol_count: 0,
        };
        names.resolve_block(&fun.block);
        SymbolTable { map: names.map, symbol_count: names.symbol_count }
    }
    fn map_to_symbol(&mut self, ident: Ident) {
        let symbol = self.scope.get(ident.as_str(self.src)).unwrap();
        self.map.insert(ident, *symbol);
    }
    fn resolve_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Let { ident, expr, .. } => {
                    if let Some(expr) = expr {
                        self.resolve_expr(&expr)
                    }
                    let symbol = self.new_symbol();
                    self.scope.insert(ident.as_str(self.src), symbol);
                    self.map_to_symbol(*ident);
                }
                Stmt::Assign { ident, expr } => {
                    self.resolve_expr(expr);
                    self.map_to_symbol(*ident);
                }
                Stmt::Return { expr } => self.resolve_expr(expr),
                Stmt::If(if_stmt) => self.resolve_if(if_stmt),
            }
        }
    }
    fn resolve_if(&mut self, if_stmt: &If) {
        self.resolve_expr(&if_stmt.cond);
        self.resolve_block(&if_stmt.if_block);
        match &if_stmt.else_block {
            Else::Block(else_block) => self.resolve_block(&else_block),
            Else::If(if_stmt) => self.resolve_if(&if_stmt),
            Else::None => (),
        }
    }
    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Ident(ident) => {
                self.map_to_symbol(*ident)
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Integer { .. } => {},
            Expr::Bool(_) => {},
            // Expr::If(if_stmt) => self.resolve_if(if_stmt),
        }
    }
    fn new_symbol(&mut self) -> Symbol {
        let symbol = Symbol(self.symbol_count);
        self.symbol_count += 1;
        symbol
    }
}

impl SymbolTable {
    pub fn ident_to_symbol(&self, ident: Ident) -> Symbol {
        *self.map.get(&ident).unwrap()
    }
    pub fn create_symbol_map<T: Default + Clone>(&self) -> SymbolMap<T> {
        SymbolMap { map: vec![T::default(); self.symbol_count as usize] }
    }
}

impl<T> SymbolMap<T> {
    pub fn update(&mut self, symbol: Symbol, item: T) {
        self.map[symbol.0 as usize] = item
    }
    pub fn get(&self, symbol: Symbol) -> &T {
        &self.map[symbol.0 as usize]
    }
    pub fn iter<'sym>(&self) -> SymbolMapIter<'_, T> {
        SymbolMapIter { symbol: Symbol(0), symbol_map: self }
    }
    pub fn map<T1>(&self, f: impl Fn(Symbol, &T) -> T1) -> SymbolMap<T1> {
        SymbolMap {
            map: self.map.iter().enumerate().map(|(i, item)| f(Symbol(i), item)).collect()
        }
    }
}

impl<'sym, T> IntoIterator for &'sym SymbolMap<T> {
    type Item = (Symbol, &'sym T);

    type IntoIter = SymbolMapIter<'sym, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct SymbolMapIter<'sym, T> {
    symbol: Symbol,
    symbol_map: &'sym SymbolMap<T>,
}

impl<'sym, T> Iterator for SymbolMapIter<'sym, T> {
    type Item = (Symbol, &'sym T);

    fn next(&mut self) -> Option<Self::Item> {
        if self.symbol.0 < self.symbol_map.map.len() {
            let item = (self.symbol, &self.symbol_map.map[self.symbol.0]);
            self.symbol.0 += 1;
            Some(item)
        } else {
            None
        }
    }
}