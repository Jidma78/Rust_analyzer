fn f(x : ()) -> i32 {
    12
}

fn main() {
    let _x = f(if true { 1; }) + 2;
    let _x = if true { 1 } + 3;
}
