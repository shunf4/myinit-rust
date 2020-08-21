use simple_error::SimpleError;

pub fn to_simple_err<T>(sth: T) -> SimpleError
where
    T: std::fmt::Display
{
    SimpleError::new(&format!("{}", sth))
}

pub fn extend_perm_exc(perm: u32) -> u32 {
    if perm > 0o777 { unreachable!(); }
    perm
        | if (perm & 0o600) != 0 { 0o100 } else { 0 }
        | if (perm & 0o060) != 0 { 0o010 } else { 0 }
        | if (perm & 0o006) != 0 { 0o001 } else { 0 }
}