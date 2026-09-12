export const CODE_EXAMPLES = [
  {
    label: 'Fibonacci function',
    value: `let fibonacci = function(x) {
    if (x == 0) {
        return 0;
    } else {
    if (x == 1) {
            return 1;
        } else {
            return fibonacci(x - 1) + fibonacci(x - 2);
        }
    }
};
let result = fibonacci(25);

print(result);`,
  },
  {
    label: 'If expression',
    value: `let a = 10;
let b = 100;

if(a > b) {
    print("a is bigger");
} else {
    print("b is bigger");
}`,
  },
  {
    label: 'Calculation function',
    value: `let calculate = function (x, y) {
  return x + y * 100
}

let result = calculate(5, 4)

print(result)`,
  },
]
