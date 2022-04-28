use std::collections::HashMap;

pub struct Symbols<'a> {
    symbol_map: HashMap<&'a str, Symbol>,
    symbols: Vec<&'a str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub u32);

impl<'a> Symbols<'a> {
    pub fn new() -> Symbols<'a> {
        Symbols { symbol_map: HashMap::new(), symbols: vec![] }
    }
    pub fn get_symbol(&mut self, ident: &'a str) -> Symbol {
        match self.symbol_map.get(ident) {
            Some(symbol) => *symbol,
            None => {
                let symbol = Symbol(self.symbols.len() as u32);
                self.symbol_map.insert(ident, symbol);
                self.symbols.push(ident);
                symbol
            }
        }
    }
    pub fn get_str(&self, symbol: Symbol) -> &'a str {
        self.symbols[symbol.0 as usize]
    }
}