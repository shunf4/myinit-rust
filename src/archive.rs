use std::cell::{RefMut, RefCell};
use std::rc::Rc;
use std::io::prelude::*;
use tar::{Archive, Builder};
use std::fs::File;
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use simple_error::SimpleError;
use std::path::Path;

use crate::util::to_simple_err;

pub struct MyInitArchive {
    inner: File,
}

pub struct MyInitLiveArchive<'a> {
    inner: Archive<GzDecoder<&'a mut File>>,
}

impl MyInitArchive {
    pub fn new(inner_file: File) -> Self {
        MyInitArchive {
            inner: inner_file,
        }
    }

    pub fn tar(&mut self) -> Result<MyInitLiveArchive, SimpleError> {
        self.inner.seek(std::io::SeekFrom::Start(0))
            .map_err(to_simple_err)?;

        Ok(MyInitLiveArchive {
            inner:
                Archive::new(
                    GzDecoder::new(
                        &mut self.inner
                    )
                )
        })
    }
}

impl<'a> MyInitLiveArchive<'a> {
    pub fn inner_mut(&mut self) -> &mut Archive<GzDecoder<&'a mut File>> {
        &mut self.inner
    }

    pub fn to_entry_path_list(&mut self) -> Result<Vec<String>, SimpleError> {
        self
            .inner
            .entries()
            .map_err(to_simple_err)?
            .map(|e| e
                .map_err(to_simple_err)
                .and_then(|e| e
                    .path()
                    .map(|p| p.into_owned())
                    .map_err(to_simple_err)
                )
                .and_then(|p| p
                    .to_str()
                    .ok_or(SimpleError::new("can't convert path to string"))
                    .map(|s| s.to_owned())
                )
            ).collect::<Result<Vec<String>, _>>()
    }

    pub fn to_entry_with_path<'b>(&mut self, path: &'b Path) -> Result<tar::Entry<GzDecoder<&'a mut File>>, SimpleError> {
        Ok(self
            .inner
            .entries()
            .map_err(to_simple_err)?
            .find(|e| {
                let result_eq = e.as_ref()
                    .map_err(to_simple_err)
                    .and_then(|e| e
                        .path()
                        .map(|p| p.into_owned())
                        .map_err(to_simple_err)
                    )
                    .map(|p| p == path
                    );

                match result_eq {
                    Ok(b) => b,
                    Err(_) => false,
                }
            })
            .ok_or(SimpleError::new("not found"))?
            .unwrap()
        )
    }
}

