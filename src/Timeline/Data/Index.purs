module Timeline.Data.Index where


-- -- | How an Event or TimeSpan is stored on-file when defined; interpretation may be different
-- data StorableIndex
--   = StorableNumber Number -- ^ ISO754
--   | StorableInt Int -- ^ 32 bit Signed Integer
--   | StorableUInt UInt -- ^ 32 bit Unsigned Integer
--   | StorableString String -- ^ UTF-8 String without --FIXME control character definition
--   | StorableDate Date -- ^ Military Date in Current Era `YYYYMMDD` -- FIXME define max and min
--   | StorableTime Time -- ^ Military Time with Second Precision `hhmmss`
--   | StorableDateTime DateTime -- ^ Military Date Time in Current Era `YYYYMMDDhhmmss` -- FIXME define max and min

-- class Indexable a where
--   toStorableIndex :: a -> StorableIndex
--   fromStorableIndex :: StorableIndex -> Either String a
