extern crate test;

#[cfg(test)]
mod tests {
    use crate::{evaluator::Evaluator, lexer::Lexer, object::Environment, parser::Parser};

    use super::*;
    use test::Bencher;

    #[bench]
    fn benchmark_adding_numbers(b: &mut Bencher) {
        b.iter(|| {
            let input = r#"
                let a = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20];
                a[0] + a[1] + a[2] + a[3] + a[4] + a[5] + a[6] + a[7] + a[8] + a[9] + a[10] + a[11] + a[12] + a[13] + a[14] + a[15] + a[16] + a[17] + a[18] + a[19];
            "#
            .to_string();
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let environment = Environment::new();
            let mut evaluator = Evaluator::new(environment);

            evaluator.eval(&program);
        });
    }
}
