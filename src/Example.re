/* Js.log("hello world"); */ /* let fizzbuzz = i =>
     switch (i mod 3, i mod 5) {
     | (0, 0) => "FizzBuzz"
     | (0, _) => "Fizz"
     | (_, 0) => "Buzz"
     | _ => string_of_int(i)
     };

   for (i in 1 to 100) {
     let a = fizzbuzz(i);
     Js.log(a);
   };

   /* Based on https://rosettacode.org/wiki/Greatest_common_divisor#OCaml */
   let rec gcd = (a, b) =>
     switch (a mod b) {
     | 0 => b
     | r => gcd(b, r)
     };

   Js.log(gcd(27, 9)); */
