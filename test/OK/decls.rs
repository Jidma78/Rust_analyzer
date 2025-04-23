fn main() {
    ;
    3 + 42;
    let _a : i32;
    let mut _b : bool;
    let _c = 42;
    let mut _d = 1e10;
    let _e : i8 = 10;
    let mut _f : f64 = 1.2_;
    fn g() { }
    fn h(_x : i32) { }
    fn i(_a : i16, mut b : f32) { b = b + 0.1; }
    fn j(mut c : f64, _d : i32,) { c = 1f64 }
    fn k(_a : i16, b : f32, mut c : f32, _d : f64) { c = b; } 
    fn l() -> i32 { 42 }
    fn m(x : i32,) -> i64 { x as i64 }
    fn n(mut a : i16, b : f32) -> bool { a = b as i16; true }
    fn o(c : f64, mut d : i32,) -> () { d = c as i32 }
    fn p(_a : i16, b : f32, mut c : f32, _d : f64) -> f32 { b + c }
}
    
    
