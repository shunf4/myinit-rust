use simple_error::SimpleError;

pub fn to_simple_err<T>(sth: T) -> SimpleError
where
    T: std::fmt::Display
{
    SimpleError::new(&format!("{}", sth))
}