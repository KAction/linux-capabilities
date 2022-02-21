{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module System.Capability (Set, known, File, encode, permitted, inheritable) where
import Data.Word (Word32, Word64)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString, word32LE)
import Data.Bits ((.&.), (.|.), shiftR)
import Data.Text (Text, unpack)
import Data.List (intercalate)
import Data.Monoid (Monoid(..))

-- | 'Set' abstract datatype represents set of capabilities, as
-- described in capabilities(7). New sets should constructed using
-- monoid instance and exported CAP_* constants.
newtype Set = Set Word64 deriving (Eq)
newtype File = File (Set, Set) deriving (Eq, Semigroup, Monoid)

-- Lens' File Set
permitted :: Functor f => (Set -> f Set) -> File -> f File
permitted k (File (p, i)) = fmap (\p' -> File (p', i)) (k p)

-- Lens' File Set
inheritable :: Functor f => (Set -> f Set) -> File -> f File
inheritable k (File (p, i)) = fmap (\i' -> File (p, i')) (k i)

-- Strictly speaking, linux represents capability set as array of
-- "__le32" of length 2 (subject to extending). But since only 40 bits
-- are allocated so far, we have quite a margin, and using Word64
-- simplifies construction code at expense of minor complication of
-- encoding/decoding code

instance Semigroup Set where
  Set x <> Set y = Set $ x .|. y

instance Monoid Set where
  mempty = Set 0

instance Show Set where
  show (Set x) =
    let names = map (unpack.fst) $ filter (\(_, Set y) -> (x .&. y) == y) known
    in "CapabilitySet {" ++ intercalate ", " names ++ "}"

splitWord64 :: Word64 -> (Word32, Word32)
splitWord64 w = (fromIntegral (w `shiftR` 32), fromIntegral w)


pattern VFS_CAP_REVISION_2 :: Word32
pattern VFS_CAP_REVISION_2 = 0x02000000

pattern VFS_CAP_FLAGS_EFFECTIVE :: Word32
pattern VFS_CAP_FLAGS_EFFECTIVE = 0x000001

encode :: File -> ByteString
encode (File (Set p, Set i)) =
  let magic = if p == 0
              then VFS_CAP_REVISION_2
              else VFS_CAP_REVISION_2 .|. VFS_CAP_FLAGS_EFFECTIVE
      (pword1, pword2) = splitWord64 p
      (iword1, iword2) = splitWord64 i
      builder = mconcat $ map word32LE [magic, pword1, iword1, pword2, iword2]
  in toStrict . toLazyByteString $ builder

-- Definitions below were generated from capability.h with
-- not-so-sophisticated awk script. Since adding new capability is
-- rare event, it is okay to maintain this list manually instead of
-- complicating build with pre-processing or template haskell.
pattern CAP_CHOWN :: Set
pattern CAP_CHOWN = Set 1

pattern CAP_DAC_OVERRIDE :: Set
pattern CAP_DAC_OVERRIDE = Set 2

pattern CAP_DAC_READ_SEARCH :: Set
pattern CAP_DAC_READ_SEARCH = Set 4

pattern CAP_FOWNER :: Set
pattern CAP_FOWNER = Set 8

pattern CAP_FSETID :: Set
pattern CAP_FSETID = Set 16

pattern CAP_KILL :: Set
pattern CAP_KILL = Set 32

pattern CAP_SETGID :: Set
pattern CAP_SETGID = Set 64

pattern CAP_SETUID :: Set
pattern CAP_SETUID = Set 128

pattern CAP_SETPCAP :: Set
pattern CAP_SETPCAP = Set 256

pattern CAP_LINUX_IMMUTABLE :: Set
pattern CAP_LINUX_IMMUTABLE = Set 512

pattern CAP_NET_BIND_SERVICE :: Set
pattern CAP_NET_BIND_SERVICE = Set 1024

pattern CAP_NET_BROADCAST :: Set
pattern CAP_NET_BROADCAST = Set 2048

pattern CAP_NET_ADMIN :: Set
pattern CAP_NET_ADMIN = Set 4096

pattern CAP_NET_RAW :: Set
pattern CAP_NET_RAW = Set 8192

pattern CAP_IPC_LOCK :: Set
pattern CAP_IPC_LOCK = Set 16384

pattern CAP_IPC_OWNER :: Set
pattern CAP_IPC_OWNER = Set 32768

pattern CAP_SYS_MODULE :: Set
pattern CAP_SYS_MODULE = Set 65536

pattern CAP_SYS_RAWIO :: Set
pattern CAP_SYS_RAWIO = Set 131072

pattern CAP_SYS_CHROOT :: Set
pattern CAP_SYS_CHROOT = Set 262144

pattern CAP_SYS_PTRACE :: Set
pattern CAP_SYS_PTRACE = Set 524288

pattern CAP_SYS_PACCT :: Set
pattern CAP_SYS_PACCT = Set 1048576

pattern CAP_SYS_ADMIN :: Set
pattern CAP_SYS_ADMIN = Set 2097152

pattern CAP_SYS_BOOT :: Set
pattern CAP_SYS_BOOT = Set 4194304

pattern CAP_SYS_NICE :: Set
pattern CAP_SYS_NICE = Set 8388608

pattern CAP_SYS_RESOURCE :: Set
pattern CAP_SYS_RESOURCE = Set 16777216

pattern CAP_SYS_TIME :: Set
pattern CAP_SYS_TIME = Set 33554432

pattern CAP_SYS_TTY_CONFIG :: Set
pattern CAP_SYS_TTY_CONFIG = Set 67108864

pattern CAP_MKNOD :: Set
pattern CAP_MKNOD = Set 134217728

pattern CAP_LEASE :: Set
pattern CAP_LEASE = Set 268435456

pattern CAP_AUDIT_WRITE :: Set
pattern CAP_AUDIT_WRITE = Set 536870912

pattern CAP_AUDIT_CONTROL :: Set
pattern CAP_AUDIT_CONTROL = Set 1073741824

pattern CAP_SETFCAP :: Set
pattern CAP_SETFCAP = Set 2147483648

pattern CAP_MAC_OVERRIDE :: Set
pattern CAP_MAC_OVERRIDE = Set 4294967296

pattern CAP_MAC_ADMIN :: Set
pattern CAP_MAC_ADMIN = Set 8589934592

pattern CAP_SYSLOG :: Set
pattern CAP_SYSLOG = Set 17179869184

pattern CAP_WAKE_ALARM :: Set
pattern CAP_WAKE_ALARM = Set 34359738368

pattern CAP_BLOCK_SUSPEND :: Set
pattern CAP_BLOCK_SUSPEND = Set 68719476736

pattern CAP_AUDIT_READ :: Set
pattern CAP_AUDIT_READ = Set 137438953472

pattern CAP_PERFMON :: Set
pattern CAP_PERFMON = Set 274877906944

pattern CAP_BPF :: Set
pattern CAP_BPF = Set 549755813888

pattern CAP_CHECKPOINT_RESTORE :: Set
pattern CAP_CHECKPOINT_RESTORE = Set 1099511627776

-- Text, not String, because I think that exporting String-based API is a crime.
--
-- I can't fix the fact that "show" is String-based, but at least I can avoid
-- exacerbating the problem.
known :: [(Text, Set)]
known = [ ("CAP_CHOWN", CAP_CHOWN)
        , ("CAP_DAC_OVERRIDE", CAP_DAC_OVERRIDE)
        , ("CAP_DAC_READ_SEARCH", CAP_DAC_READ_SEARCH)
        , ("CAP_FOWNER", CAP_FOWNER)
        , ("CAP_FSETID", CAP_FSETID)
        , ("CAP_KILL", CAP_KILL)
        , ("CAP_SETGID", CAP_SETGID)
        , ("CAP_SETUID", CAP_SETUID)
        , ("CAP_SETPCAP", CAP_SETPCAP)
        , ("CAP_LINUX_IMMUTABLE", CAP_LINUX_IMMUTABLE)
        , ("CAP_NET_BIND_SERVICE", CAP_NET_BIND_SERVICE)
        , ("CAP_NET_BROADCAST", CAP_NET_BROADCAST)
        , ("CAP_NET_ADMIN", CAP_NET_ADMIN)
        , ("CAP_NET_RAW", CAP_NET_RAW)
        , ("CAP_IPC_LOCK", CAP_IPC_LOCK)
        , ("CAP_IPC_OWNER", CAP_IPC_OWNER)
        , ("CAP_SYS_MODULE", CAP_SYS_MODULE)
        , ("CAP_SYS_RAWIO", CAP_SYS_RAWIO)
        , ("CAP_SYS_CHROOT", CAP_SYS_CHROOT)
        , ("CAP_SYS_PTRACE", CAP_SYS_PTRACE)
        , ("CAP_SYS_PACCT", CAP_SYS_PACCT)
        , ("CAP_SYS_ADMIN", CAP_SYS_ADMIN)
        , ("CAP_SYS_BOOT", CAP_SYS_BOOT)
        , ("CAP_SYS_NICE", CAP_SYS_NICE)
        , ("CAP_SYS_RESOURCE", CAP_SYS_RESOURCE)
        , ("CAP_SYS_TIME", CAP_SYS_TIME)
        , ("CAP_SYS_TTY_CONFIG", CAP_SYS_TTY_CONFIG)
        , ("CAP_MKNOD", CAP_MKNOD)
        , ("CAP_LEASE", CAP_LEASE)
        , ("CAP_AUDIT_WRITE", CAP_AUDIT_WRITE)
        , ("CAP_AUDIT_CONTROL", CAP_AUDIT_CONTROL)
        , ("CAP_SETFCAP", CAP_SETFCAP)
        , ("CAP_MAC_OVERRIDE", CAP_MAC_OVERRIDE)
        , ("CAP_MAC_ADMIN", CAP_MAC_ADMIN)
        , ("CAP_SYSLOG", CAP_SYSLOG)
        , ("CAP_WAKE_ALARM", CAP_WAKE_ALARM)
        , ("CAP_BLOCK_SUSPEND", CAP_BLOCK_SUSPEND)
        , ("CAP_AUDIT_READ", CAP_AUDIT_READ)
        , ("CAP_PERFMON", CAP_PERFMON)
        , ("CAP_BPF", CAP_BPF)
        , ("CAP_CHECKPOINT_RESTORE", CAP_CHECKPOINT_RESTORE)
        ]
