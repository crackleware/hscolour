{-# LANGUAGE CPP #-}

module Util where

import System.IO

set_utf8_io_enc :: Handle -> IO Handle
set_utf8_io_enc h =
#if MIN_VERSION_base(4,2,0)
    hSetEncoding h utf8 >>
#endif
    return h
