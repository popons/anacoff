import Data.Attoparsec
import Data.Attoparsec.Binary
import qualified Data.ByteString.Char8 as C
import Data.Binary;
import System.Time
import System.Environment(getArgs)
import Control.Monad(forM_, replicateM)
import Data.Char(chr,isAlpha)
import Data.Bits
import Text.Show.Pretty
import Data.Maybe(fromMaybe)
import Text.Printf(printf)
import Control.Applicative
import Data.List(intercalate)
import System.IO(hPutStrLn,withFile, IOMode(WriteMode))

-- Header Flags
data HeadFlag =
        F_RELFLG | F_EXEC | F_LNNO | F_LSYMS|
        F_LITTLE | F_BIG | F_SYMMERGE | F_UNKNOWN Word16
        deriving(Show, Eq)

-- Section Header Flags
data SectHeaderFlag =
        STYP_REG | STYP_DSECT | STYP_NOLOAD | STYP_GROUP | STYP_PAD |
        STYP_COPY | STYP_TEXT | STYP_DATA | STYP_BSS | STYP_BLOCK | STYP_PASS |
        STYP_CLINK | STYP_VECTOR | STYP_PADDED | STYP_UNKNOWN Word32
        deriving(Show, Eq)

--
data DeviceFamily =
        TMS470  | TMS320C5400 | TMS320C6000 | TMS320C5500 |
        TMS320C2800 | MSP430 | TMS320C5500P | TMS_UNKNOWN Word16
        deriving(Show)

(<<) :: Bits a => a -> Int -> a
x << s = x `shiftL` s

toBits :: (Num b, Bits b) => b -> [b]
toBits x = filter ((0/=) . (x .&.)) $ map (1<<) [0..31]

toFlags :: Word16 -> [HeadFlag]
toFlags = map toFlag . toBits

toSectFlags :: Word32 -> [SectHeaderFlag]
toSectFlags = map toSectFlag . toBits

toFlag :: Word16 -> HeadFlag
toFlag k = fromMaybe (F_UNKNOWN k) $ lookup k
    [   (0x0001, F_RELFLG)
    ,   (0x0002, F_EXEC)
    ,   (0x0004, F_LNNO)
    ,   (0x0008, F_LSYMS)
    ,   (0x0100, F_LITTLE)
    ,   (0x0200, F_BIG)
    ,   (0x1000, F_SYMMERGE)]

toSectFlag :: Word32 -> SectHeaderFlag
toSectFlag k = fromMaybe (STYP_UNKNOWN k) $ lookup k
       [(0x00000000, STYP_REG)
    ,   (0x00000001, STYP_DSECT)
    ,   (0x00000002, STYP_NOLOAD)
    ,   (0x00000004, STYP_GROUP)
    ,   (0x00000008, STYP_PAD)
    ,   (0x00000010, STYP_COPY)
    ,   (0x00000020, STYP_TEXT)
    ,   (0x00000040, STYP_DATA)
    ,   (0x00000080, STYP_BSS)
    ,   (0x00001000, STYP_BLOCK)
    ,   (0x00002000, STYP_PASS)
    ,   (0x00004000, STYP_CLINK)
    ,   (0x00008000, STYP_VECTOR)
    ,   (0x00010000, STYP_PADDED)]

toDeviceFamily :: Word16 -> DeviceFamily
toDeviceFamily k = fromMaybe (TMS_UNKNOWN k) $ lookup k
        [(0x0097, TMS470)
    ,    (0x0098, TMS320C5400)
    ,    (0x0099, TMS320C6000)
    ,    (0x009c, TMS320C5500)
    ,    (0x009d, TMS320C2800)
    ,    (0x00a0, MSP430)
    ,    (0x00a1, TMS320C5500P)]

data COFFHeader = COFFHeader {
        version             :: Word16
    ,   secHeaderNum        :: Word16
    ,   timestamp           :: ClockTime
    ,   symTableAddr        :: Word32
    ,   symTableEntry       :: Word32
    ,   optHeadLen          :: Word16
    ,   flags               :: [HeadFlag]
    ,   targetID            :: DeviceFamily
    }

data OptionalHeader = OptionalHeader {
        optMagic            :: Word16
    ,   versionStamp        :: Word16
    ,   codeBytes           :: Word32
    ,   iniDataBytes        :: Word32
    ,   unIniDataBytes      :: Word32
    ,   entryPoint          :: Word32
    ,   beginCode           :: Word32
    ,   beginIniData        :: Word32
    }

data SectionHeader = SectionHeader {
        sectName            :: String   -- 8 bytes
    ,   sectPhysAddr        :: Word32
    ,   sectVirtAddr        :: Word32
    ,   sectSizeWordN       :: Word32
    ,   filePtrRaw          :: Word32
    ,   filePtrRelocEntry   :: Word32
    ,   reserved1           :: Word32
    ,   numOfRelocEntry     :: Word32
    ,   reserved2           :: Word32
    ,   sectionFlags        :: [SectHeaderFlag]
    ,   reserved3           :: Word16
    ,   memPageNumber       :: Word16
    }

data COFF = COFF {
        coffHeader     :: COFFHeader
    ,   optionalHeader :: Maybe OptionalHeader
    ,   sectionHeaders :: [SectionHeader]
    }

instance Show COFFHeader where
    show (COFFHeader v s ts sa sr o f id) =
        printf "\nCOFFHeader {" ++
        printf "\n  version       = 0x%02X" v ++
        printf "\n  secHeaderNum  = %d" s ++
               "\n  timestamp     = " ++ show ts ++
        printf "\n  symTableAddr  = 0x%08X" sa ++
        printf "\n  symTableEntry = %d" sr ++
        printf "\n  optHeadLen    = %d" o ++
               "\n  flags         = " ++  show f ++
               "\n  targetID      = " ++  show id ++ " } "

instance Show OptionalHeader where
    show (OptionalHeader o v c i u e bc bd) =
        printf "\nOptionalHeader {" ++
        printf "\n  optMagic       = 0x%04X" o ++
        printf "\n  versionStamp   = 0x%04X" v ++
               "\n  codeBytes      = " ++ show c ++
        printf "\n  iniDataBytes   = " ++ show i ++
        printf "\n  unIniDataBytes = " ++ show u ++
        printf "\n  entryPoint     = 0x%08X" e ++
        printf "\n  beginCode      = 0x%08X" bc ++
        printf "\n  beginIniData   = 0x%08X" bd ++ " }"

instance Show SectionHeader where
    show (SectionHeader n pa va wn fpr fpre r1 nr r2 fs r3 mpn) =
        printf "\nSectionHeader {" ++
        printf "\n  sectName            = " ++ show n ++
        printf "\n  sectPhysAddr        = 0x%08X" pa ++
        printf "\n  sectVirtAddr        = 0x%08X" va ++
        printf "\n  sectSizeWordN       = %d" wn ++
        printf "\n  filePtrRaw          = 0x%08X" fpr ++
        printf "\n  filePtrRelocEntry   = %d" fpre ++
        printf "\n  reserved1           = 0x%08X" r1 ++
        printf "\n  numOfRelocEntry     = %d" nr ++
        printf "\n  reserved2           = 0x%08X" r2 ++
        printf "\n  sectionFlags        = " ++ show fs ++
        printf "\n  reserved3           = 0x%08X" r3 ++
        printf "\n  memPageNumber       = %d" mpn

instance Show COFF where
    show (COFF ch o shs) =
        printf "\nCOFF {" ++
        printf "\n  coffHeader          = " ++ show ch ++
        printf "\n  optionalHeader      = " ++ show o ++
        printf "\n  sectionHeaders      = " ++ show shs ++ " }"

d16 :: Parser Word16
d32 :: Parser Word32
d8 :: Parser Word8
d16 = anyWord16le
d32 = anyWord32le
d8 = anyWord8

sectHeaderParser :: Parser SectionHeader
sectHeaderParser =
    SectionHeader
        <$> (map (chr . fromIntegral) <$> replicateM 8 d8)
        <*> d32 <*> d32 <*> d32 <*> d32 <*> d32
        <*> d32 <*> d32 <*> d32 <*> (toSectFlags <$> d32)
        <*> d16 <*> d16

optHeadParser :: Parser OptionalHeader
optHeadParser =
    OptionalHeader
        <$> d16 <*> d16
        <*> d32 <*> d32 <*> d32
        <*> d32 <*> d32 <*> d32

coffHeadParser :: Parser COFFHeader
coffHeadParser =
    COFFHeader
        <$> d16 <*> d16
        <*> (((`TOD` 0) . toInteger) <$> d32) <*> d32 <*> d32
        <*> d16 <*> (toFlags <$> d16) <*> (toDeviceFamily <$> d16)

parser :: Parser COFF
parser = do
    coffHeader <- coffHeadParser
    optHeader <- if optHeadLen coffHeader == 0
                     then return Nothing
                     else Just <$> optHeadParser
    sectHeaders <- replicateM
                        (fromIntegral $ secHeaderNum coffHeader)
                        sectHeaderParser
    return $ COFF coffHeader optHeader sectHeaders

parseRight :: C.ByteString -> COFF
parseRight xs = case parseOnly parser xs of Right x -> x

printer :: [SectionHeader -> String]
printer = [ filter isAlpha . sectName, printf "0x%X" . sectPhysAddr, printf "0x%X" . sectVirtAddr, show . sectSizeWordN,
            printf "0x%X" . filePtrRaw, printf "0x%X" . filePtrRelocEntry, show . reserved1, show . numOfRelocEntry,
            show . reserved2, show . sectionFlags, show . reserved3, show . memPageNumber]

headers :: [String]
headers = ["sectName", "sectPhysAddr", "sectVirtAddr", "sectSizeWordN",
          "filePtrRaw", "filePtrRelocEntry", "reserved1", "numOfRelocEntry",
          "reserved2", "sectionFlags", "reserved3", "memPageNumber"]

output :: COFF -> IO ()
output info = do
    putStrLn $ ppShow info
    let shs = sectionHeaders info
    withFile "aho.tsv" WriteMode $ \h -> do
        hPutStrLn h $ intercalate "\t" headers
        forM_ shs $ \sh ->
            hPutStrLn h $ intercalate "\t" $ map ($sh) printer

main :: IO ()
main = do
    files <- getArgs
    forM_ files $ \file -> do
        xs <- C.readFile file
        case parseOnly parser xs of
            Right info -> output info
            Left er -> putStrLn er

