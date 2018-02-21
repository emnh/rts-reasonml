let sum = f => f(0);

let arg = (x, acc, g) => g(acc + x);

let z = a => a;

sum(arg(1), z);

sum(arg(1), arg(2), arg(3), z);
