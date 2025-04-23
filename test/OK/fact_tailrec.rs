fn fact(n : u32) -> i32 {
    fn fact(n : i32, accu : i32) -> i32 {
        let r = if n == 0 { accu }
        else { fact(n - 1, accu * n) };
        r
    }

    fact(n, 1)
}
    


fn main() {
   let _ = fact(10);
}
