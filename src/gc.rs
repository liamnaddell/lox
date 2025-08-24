//provides an allocator for Value2
//should have mark-sweep algorithm in it.
//vm will call gc::mark_sweep(roots), clean da garbage
pub struct Gc {
}
impl Gc {
    pub fn new() -> Gc {
        return Gc{
        };
    }
}
