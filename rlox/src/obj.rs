use crate::error::*;

#[derive(Clone)]
pub enum ObjType {
    String(ObjString),
}

#[derive(Clone)]
pub struct Obj {
    pub obj_type: ObjType,
}

pub struct ObjString {
    length: usize,
    ptr: *mut u8,
}

impl Clone for ObjString {
    fn clone(&self) -> Self {
        Self {
            length: self.length,
            ptr: self.ptr.clone(),
        }
    }
}

impl ObjString {
    pub fn new() -> ObjString {
        ObjString {
            length: 0,
            ptr: std::ptr::null_mut(),
        }
    }
    pub fn equal(&self, other: &str) -> bool {
        if self.length != other.len() {
            return false;
        } else {
            return self.str_bytes() == other.as_bytes();
        }
    }
    pub fn is_empty(&self) -> bool {
        return self.length == 0 && self.ptr.is_null()
    }
    pub fn length(&self) -> usize {
        return self.length
    }

    fn str_bytes(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.length) }
    }

    fn str_str(&self) -> Result<String> {
        let str_bytes = self.str_bytes();
        match String::from_utf8(str_bytes.to_vec()) {
            Ok(string) =>  Ok(string),
            Err(_) => Err(CompileError::from_str(
                0,
                "failed to get strings out of your bytes",
            )),
        }
    }

}
