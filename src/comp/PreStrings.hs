{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module PreStrings where
import FStringCompat
import Util(itos)


fsEmpty            = mkFString ""
fsUnderscore       = mkFString "_"
fsUnderUnder       = mkFString "__"
fsDot              = mkFString "."
fsRArrow           = mkFString "->"
fsBar              = mkFString "|"
fsStar             = mkFString "*"
fsSlash            = mkFString "/"
fsPercent          = mkFString "%"
fsStarStar         = mkFString "**"
fsHash             = mkFString "#"
fsTilde            = mkFString "~"
fsTyJoin           = mkFString "_$"
fsAssign           = mkFString ":="
fsDollar           = mkFString "$"
fsLT               = mkFString "<"
fsGT               = mkFString ">"
fsLTGT             = mkFString "<>"
fsComma            = mkFString ","
fsPlus             = mkFString "+"
fsMinus            = mkFString "-"
fsLsh              = mkFString "<<"
fsRsh              = mkFString ">>"
fsLtEq             = mkFString "<="
fsGtEq             = mkFString ">="
-- | Scheduling conflict operator for Classic
fsConfOp           = mkFString "><"
fsPrelude          = mkFString "Prelude"
fsPreludeBSV       = mkFString "PreludeBSV"
fsPrimUnit         = mkFString "PrimUnit"
fsBit              = mkFString "Bit"
fsInt              = mkFString "Int"
fsUInt             = mkFString "UInt"
fsClock            = mkFString "Clock"
fsClockOsc         = mkFString "osc"
fsClockGate        = mkFString "gate"
fsReset            = mkFString "Reset"
fsInout            = mkFString "Inout"
fsInout_           = mkFString "Inout_"
fsPrimInoutCast    = mkFString "primInoutCast"
fsPrimInoutUncast  = mkFString "primInoutUncast"
fsPrimInoutCast0   = mkFString "primInoutCast0"
fsPrimInoutUncast0 = mkFString "primInoutUncast0"
fsInteger          = mkFString "Integer"
fsReal             = mkFString "Real"
fsMonad            = mkFString "Monad"
fsString           = mkFString "String"
fsChar             = mkFString "Char"
fsHandle           = mkFString "Handle"
fsBufferMode       = mkFString "BufferMode"
fsNoBuffering      = mkFString "NoBuffering"
fsLineBuffering    = mkFString "LineBuffering"
fsBlockBuffering   = mkFString "BlockBuffering"
fsFmt              = mkFString "Fmt"
fsPrimFmtConcat    = mkFString "primFmtConcat"
fsPrimPriMux       = mkFString "primPriMux"
fsFormat           = mkFString "$format"
fsFShow            = mkFString "FShow"
fsfshow            = mkFString "fshow"
fsBool             = mkFString "Bool"
fsPrimFst          = mkFString "fst"
fsPrimSnd          = mkFString "snd"
fsPrimPair         = mkFString "PrimPair"
fsTrue             = mkFString "True"
fsFalse            = mkFString "False"
fsSizeOf           = mkFString "SizeOf"
fsEq               = mkFString "Eq"
fsBits             = mkFString "Bits"
fsLiteral          = mkFString "Literal"
fsRealLiteral      = mkFString "RealLiteral"
fsSizedLiteral     = mkFString "SizedLiteral"
fsStringLiteral    = mkFString "StringLiteral"
fsPrimParam        = mkFString "PrimParam"
fsPrimPort         = mkFString "PrimPort"
fsPrimToParam      = mkFString "primToParam"
fsPrimToPort       = mkFString "primToPort"
fsPrimSeqCond      = mkFString "primSeqCond"
fsPrimDeepSeqCond  = mkFString "primDeepSeqCond"
fsClsDeepSeqCond   = mkFString "PrimDeepSeqCond"
fsUndefined        = mkFString "PrimMakeUndefined"
fsBuildUndef       = mkFString "primBuildUndefined"
fsMakeUndef        = mkFString "primMakeUndefined"
fsRawUndef         = mkFString "primMakeRawUndefined"
fsEqual            = mkFString "=="
fsNotEqual         = mkFString "/="
fsPrimOrd          = mkFString "primOrd"
fsPrimChr          = mkFString "primChr"
fsPack             = mkFString "pack"
fsUnpack           = mkFString "unpack"
fsVReg             = mkFString "VReg"
fsReg              = mkFString "Reg"
fsRWire            = mkFString "RWire"
fsPulseWire        = mkFString "PulseWire"
fsFIFO             = mkFString "FIFO"
fsFIFOF            = mkFString "FIFOF"
fsAction           = mkFString "Action"
fsPrimAction       = mkFString "PrimAction"
fsToPrimAction     = mkFString "toPrimAction"
fsFromPrimAction   = mkFString "fromPrimAction"
fsToActionValue_   = mkFString "toActionValue_"
fsFromActionValue_ = mkFString "fromActionValue_"
fsActionValue      = mkFString "ActionValue"
fsAVValue          = mkFString "avValue"
fsAVAction         = mkFString "avAction"
fsActionValue_     = mkFString "ActionValue_"
fsAVValue_         = mkFString "avValue_"
fsAVAction_        = mkFString "avAction_"
fs__value          = mkFString "__value"
fs__action         = mkFString "__action"
fsMethodReturn     = mkFString "method_return"
fsRules            = mkFString "Rules"
fsSchedPragma      = mkFString "SchedPragma"
fsModule           = mkFString "Module"
fsId               = mkFString "Id__"
fsPred             = mkFString "Pred__"
fsIsModule         = mkFString "IsModule"
fsPrimModule       = mkFString "primModule"
fsName             = mkFString "Name__"
fsGetModuleName    = mkFString "primGetModuleName"
fsPosition         = mkFString "Position__"
fsType             = mkFString "Type"
fsTypeOf           = mkFString "typeOf"
fsSavePortType     = mkFString "primSavePortType"
fsPrintType        = mkFString "printType"
fsNoPosition       = mkFString "noPosition"
fsPrimGetEvalPosition  = mkFString "primGetEvalPosition"
fsPrimGetName      = mkFString "primGetName"
fsPrimGetParamName = mkFString "primGetParamName"
fsPrimJoinNames    = mkFString "primJoinNames"
fsPrimExtNameIdx   = mkFString "primExtendNameIndex"
fsSetStateName     = mkFString "setStateName"
fsForceIsModule    = mkFString "forceIsModule"
fsAttributes       = mkFString "Attributes__"
fsSetStateAttrib   = mkFString "setStateAttrib"
fsAdd              = mkFString "Add"
fsMax              = mkFString "Max"
fsMin              = mkFString "Min"
fsLog              = mkFString "Log"
fsMul              = mkFString "Mul"
fsDiv              = mkFString "Div"
fsNumEq            = mkFString "NumEq"
fsAnd              = mkFString "&&"
fsOr               = mkFString "||"
fsBitAnd           = mkFString "&"
fsBitOr            = mkFString "|"
fsCaret            = mkFString "^"
fsTildeCaret       = mkFString "~^"
fsCaretTilde       = mkFString "^~"
fsReduceAnd        = mkFString "reduceAnd"
fsReduceOr         = mkFString "reduceOr"
fsReduceXor        = mkFString "reduceXor"
fsReduceNand       = mkFString "reduceNand"
fsReduceNor        = mkFString "reduceNor"
fsReduceXnor       = mkFString "reduceXnor"
fsNot              = mkFString "not"
fsPrimSplit        = mkFString "primSplit"
fsPrimConcat       = mkFString "primConcat"
fsPrimMul          = mkFString "primMul"
fsPrimQuot         = mkFString "primQuot"
fsPrimRem          = mkFString "primRem"
fsPrimTrunc        = mkFString "primTrunc"
fs_lam             = mkFString "_lam"
fs_if              = mkFString "_if"
fs_read            = mkFString "_read"
fs_write           = mkFString "_write"
fsAsIfc            = mkFString "asIfc"
fsAsReg            = mkFString "asReg"
fsRead             = mkFString "read"
fsRegWrite         = mkFString "write"
fsExtract          = mkFString "primExtract"
fsFromInteger      = mkFString "fromInteger"
fsFromReal         = mkFString "fromReal"
fsFromSizedInteger = mkFString "fromSizedInteger"
fsFromString       = mkFString "fromString"
fsPrimCharToString = mkFString "primCharToString"
fs_case            = mkFString "_case"
fsPrimWhen         = mkFString "_when"
fsBind             = mkFString "bind"
fsBind_            = mkFString "bind_"
fsReturn           = mkFString "return"
fs_x               = mkFString "_x"
fs_y               = mkFString "_y"
fs_fun             = mkFString "_fun"
fs_forallb         = mkFString "_forallb"
fsBounded          = mkFString "Bounded"
fsMaxBound         = mkFString "maxBound"
fsMinBound         = mkFString "minBound"
fsDefaultValue     = mkFString "DefaultValue"
fs_defaultValue    = mkFString "defaultValue"
fsPrimSplitFst     = mkFString "primSplitFst"
fsPrimSplitSnd     = mkFString "primSplitSnd"
fsTo               = mkFString "to"
fsFrom             = mkFString "from"
fs_t               = mkFString "_t"
fsMux              = mkFString "MUX_"
fsMuxSel           = mkFString "SEL"
fsMuxPreSel        = mkFString "PSEL"
fsMuxVal           = mkFString "VAL"
fsEnable           = mkFString "EN_"
fs_rdy             = mkFString "RDY_"
fs_rl              = mkFString "RL_"
fs_unnamed         = mkFString "unnamed"
s_unnamed          = "unnamed"
fs_T               = mkFString "_T"
fs_F               = mkFString "_F"
fsCanFire          = mkFString "CAN_FIRE_"
fsWillFire         = mkFString "WILL_FIRE_"
fsCLK              = mkFString "CLK"
fsCLK_GATE         = mkFString "CLK_GATE"
-- fsRSTN             = mkFString "RST"
fsDefaultClock     = mkFString "default_clock"
fsDefaultReset     = mkFString "default_reset"
fsPrimStringConcat = mkFString "primStringConcat"
fsNoinline         = mkFString "noinline"
fsLiftM            = mkFString "liftM"
fsLiftModule       = mkFString "liftModule"
fsPrimBAnd         = mkFString "primBAnd"
fsPrimBOr          = mkFString "primBOr"
fsPrimBNot         = mkFString "primBNot"
fsPrimIf           = mkFString "primIf"
fsPrimCase         = mkFString "primCase"
fsPrimArrayDynSelect = mkFString "primArrayDynSelect"
fsPrimBuildArray   = mkFString "primBuildArray"
fsPrimSelect       = mkFString "primSelect"
fsPrimSelectable   = mkFString "PrimSelectable"
fsPrimUpdateable   = mkFString "PrimUpdateable"
fsPrimWriteable    = mkFString "PrimWriteable"
fsPrimSelectFn     = mkFString "primSelectFn"
fsPrimUpdateFn     = mkFString "primUpdateFn"
fsPrimWriteFn      = mkFString "primWriteFn"
fsPrimIndex        = mkFString "PrimIndex"
fsPrimUpdateRangeFn = mkFString "primUpdateRangeFn"
fsPrimZeroExt      = mkFString "primZeroExt"
fsPrimSignExt      = mkFString "primSignExt"
fsPrimValueOf      = mkFString "primValueOf"
fsPrimStringOf     = mkFString "primStringOf"
fsStringProxy      = mkFString "StringProxy"
fsPrimJoinRules    = mkFString "primJoinRules"
fsPrimNoRules      = mkFString "primNoRules"
fsPrimRule         = mkFString "primRule"
fsPrimAddSchedPragmas = mkFString "primAddSchedPragmas"
fsPrimJoinActions  = mkFString "primJoinActions"
fsPrimNoActions    = mkFString "primNoActions"
fsPrimNoExpIf      = mkFString "primNoExpIf"
fsPrimExpIf        = mkFString "primExpIf"
fsPrimNosplitDeep  = mkFString "primNosplitDeep"
fsPrimSplitDeep    = mkFString "primSplitDeep"
fsSplitDeepAV      = mkFString "splitDeepAV"
fsNosplitDeepAV    = mkFString "nosplitDeepAV"
fsPrimInv          = mkFString "primInv"
fsPrimEQ           = mkFString "primEQ"
fsPrimULE          = mkFString "primULE"
fsPrimULT          = mkFString "primULT"
fsPrimSLE          = mkFString "primSLE"
fsPrimSLT          = mkFString "primSLT"
fsPrimSL           = mkFString "primSL"
fsPrimSRL          = mkFString "primSRL"
fsPrimAdd          = mkFString "primAdd"
fsPrimSub          = mkFString "primSub"
fsClsUninitialized = mkFString "PrimMakeUninitialized"
fsPrimUninitialized = mkFString "primUninitialized"
fsPrimMakeUninitialized = mkFString "primMakeUninitialized"
fsPrimRawUninitialized= mkFString "primMakeRawUninitialized"
fsPrimPoisonedDef  = mkFString "primPoisonedDef"
fsChangeSpecialWires = mkFString "changeSpecialWires"
fsPrimSetSelPosition = mkFString "primSetSelPosition"
fsTAdd             = mkFString "TAdd"
fsTSub             = mkFString "TSub"
fsTMul             = mkFString "TMul"
fsTDiv             = mkFString "TDiv"
fsTLog             = mkFString "TLog"
fsTExp             = mkFString "TExp"
fsTMax             = mkFString "TMax"
fsTMin             = mkFString "TMin"
fsStaticAssert     = mkFString "staticAssert"
fsDynamicAssert    = mkFString "dynamicAssert"
fsContinuousAssert = mkFString "continuousAssert"
fs_staticAssert    = mkFString "_staticAssert"
fs_dynamicAssert   = mkFString "_dynamicAssert"
fs_continuousAssert= mkFString "_continuousAssert"
fsAddRules         = mkFString "addRules"
fsASSERT           = mkFString "ASSERT"
fsFire             = mkFString "fire"
fsEnabled          = mkFString "enabled"
fsNo               = mkFString "no"
fsImplicit         = mkFString "implicit"
fsConditions       = mkFString "conditions"
fsCan              = mkFString "can"
fsSchedule         = mkFString "schedule"
fsFirst            = mkFString "first"
fsClockCrossing    = mkFString "clock-crossing"
fsAggressiveImplicitConditions   = mkFString "aggressive_implicit_conditions"
fsConservativeImplicitConditions = mkFString "conservative_implicit_conditions"
fsNoWarn           = mkFString "no_warn"
fsWarnAllConflicts = mkFString "warn_all_conflicts"
fsRule             = mkFString "rule"
fsMkRegU           = mkFString "mkRegU"
fsPrimFix          = mkFString "primFix"
fsMfix             = mkFString "mfix"
fsFmap             = mkFString "fmap"
fsNegate           = mkFString "negate"
fsIdentity         = mkFString "id"
fsInvert           = mkFString "invert"
fsEmptyIfc         = mkFString "Empty"
fs1                = mkFString "1"
fsB                = mkFString "B"
fsR                = mkFString "R"
fsExposeCurrentClock = mkFString "exposeCurrentClock"
fsExposeCurrentReset = mkFString "exposeCurrentReset"
fsNoClock          = mkFString "noClock"
fsNoReset          = mkFString "noReset"
fsPrimReplaceClockGate = mkFString "primReplaceClockGate"
fsVRWireN          = mkFString "VRWireN"
fsVmkRWire1        = mkFString "vMkRWire1"
fsWGet             = mkFString "wget"
fsWSet             = mkFString "wset"
fsWHas             = mkFString "whas"
fsSend             = mkFString "send"
fsFIFO_notFull     = mkFString "i_notFull"
fsFIFO_notEmpty    = mkFString "i_notEmpty"
fsFIFOEnq          = mkFString "enq"
fsFIFODeq          = mkFString "deq"
fsFIFOFirst        = mkFString "first"

-- XXX low ASCII only, please...
sAcute             = "__"
fsAcute            = mkFString sAcute
fsTheResult        = mkFString "_theResult__"
fsF                = mkFString "_f__"
fsM                = mkFString "_m__"
fsC                = mkFString "_c__"
fsClk              = mkFString "_clk__"
fsRst              = mkFString "_rst__"
fsList             = mkFString "List"
fsPrimArray        = mkFString "Array"
fsPrimArrayNew     = mkFString "primArrayNew"
fsPrimArrayNewU     = mkFString "primArrayNewU"
fsPrimArrayLength  = mkFString "primArrayLength"
fsPrimArrayInitialize  = mkFString "primArrayInitialize"
fsPrimArrayCheck   = mkFString "primArrayCheck"
fsListN            = mkFString "ListN"
fsVector           = mkFString "Vector"
fsToVector         = mkFString "toVector"
fsToListN          = mkFString "toListN"
fsSAction          = mkFString "SAction"
fsSActionValue     = mkFString "SActionValue"
fsStmtify          = mkFString "stmtify"
fsCallServer       = mkFString "callServer"
fsSIf1             = mkFString "SIf1"
fsSIf2             = mkFString "SIf2"
fsSAbtIf1          = mkFString "SAbtIf1"
fsSAbtIf2          = mkFString "SAbtIf2"
fsSRepeat          = mkFString "SRepeat"
fsSWhile           = mkFString "SWhile"
fsSFor             = mkFString "SFor"
fsSSeq             = mkFString "SSeq"
fsSPar             = mkFString "SPar"
fsSLabel           = mkFString "SLabel"
fsSJump            = mkFString "SJump"
fsSNamed           = mkFString "SNamed"
fsS                = mkFString "S"
fsStmt             = mkFString "stmt"
fsSBreak           = mkFString "SBreak"
fsSContinue        = mkFString "SContinue"
fsSReturn          = mkFString "SReturn"
fsCons             = mkFString "Cons"
fsConcat           = mkFString "concat"
fsNil              = mkFString "Nil"
fsNothing          = mkFString "Nothing"
fsSprime           = mkFString "_s__"
fsMaybe            = mkFString "Maybe"
fsInvalid          = mkFString "Invalid"
fsValid            = mkFString "Valid"
-- | Names used for tuple fields internally?
fsTuples = map mkFString ["_"++ itos i | i <- [1..25::Int]]
-- | Names exposed to the BSV user
fsTuple2           = mkFString "Tuple2"
fsTuple3           = mkFString "Tuple3"
fsTuple4           = mkFString "Tuple4"
fsTuple5           = mkFString "Tuple5"
fsTuple6           = mkFString "Tuple6"
fsTuple7           = mkFString "Tuple7"
fsTuple8           = mkFString "Tuple8"
fsConstAllBitsSet  = mkFString "constantWithAllBitsSet"
fsConstAllBitsUnset= mkFString "constantWithAllBitsUnset"
fs_the_                   = mkFString "the_"
s__fire            = "_fire"
fs__fire           = mkFString s__fire


fsPrimError = mkFString "primError"

-- XXX should be a system task?
fsError = mkFString "error"

-- system task strings
fsFinish    = mkFString "$finish"
fsStop      = mkFString "$stop"
fsDisplay   = mkFString "$display"
fsDisplayh  = mkFString "$displayh"
fsDisplayb  = mkFString "$displayb"
fsDisplayo  = mkFString "$displayo"
fsWrite     = mkFString "$write"
fsWriteh    = mkFString "$writeh"
fsWriteb    = mkFString "$writeb"
fsWriteo    = mkFString "$writeo"

fsFDisplay   = mkFString "$fdisplay"
fsFDisplayh  = mkFString "$fdisplayh"
fsFDisplayb  = mkFString "$fdisplayb"
fsFDisplayo  = mkFString "$fdisplayo"
fsFWrite     = mkFString "$fwrite"
fsFWriteh    = mkFString "$fwriteh"
fsFWriteb    = mkFString "$fwriteb"
fsFWriteo    = mkFString "$fwriteo"
fsSWriteAV   = mkFString "$swriteAV"
fsSWrite     = mkFString "$swrite"
fsSWritehAV  = mkFString "$swritehAV"
fsSWriteh    = mkFString "$swriteh"
fsSWritebAV  = mkFString "$swritebAV"
fsSWriteb    = mkFString "$swriteb"
fsSWriteoAV  = mkFString "$swriteoAV"
fsSWriteo    = mkFString "$swriteo"
fsSFormatAV  = mkFString "$sformatAV"
fsSFormat    = mkFString "$sformat"

fsErrorTask  = mkFString "$error"
fsWarnTask   = mkFString "$warning"
fsInfoTask   = mkFString "$info"
fsFatalTask   = mkFString "$fatal"

fsSVA          = mkFString "$SVA"
fsSvaParam     = mkFString "SvaParam"
fsSvaBool      = mkFString "SvaBool"
fsSvaNumber    = mkFString "SvaNumber"

fsSVAsampled   = mkFString "$sampled"
fsSVArose      = mkFString "$rose"
fsSVAfell      = mkFString "$fell"
fsSVAstable    = mkFString "$stable"
fsSVApast      = mkFString "$past"
fsSVAonehot    = mkFString "$onehot"
fsSVAonehot0   = mkFString "$onehot0"
fsSVAisunknown = mkFString "$isunknown"
fsSVAcountones = mkFString "$countones"

fsRandom    = mkFString "$random"

fsDumpon    = mkFString "$dumpon"
fsDumpoff   = mkFString "$dumpoff"
fsDumpvars  = mkFString "$dumpvars"
fsDumpall   = mkFString "$dumpall"
fsDumplimit = mkFString "$dumplimit"
fsDumpflush = mkFString "$dumpflush"
fsDumpfile  = mkFString "$dumpfile"
fsSigned    = mkFString "$signed"
sSigned     = "$signed"
fsUnsigned  = mkFString "$unsigned"
sUnsigned   = "$unsigned"
fsTime      = mkFString "$time"
fsSTime     = mkFString "$stime"
fsFOpen     = mkFString "$fopen"
fsFGetc     = mkFString "$fgetc"
fsUngetc    = mkFString "$ungetc"
fsFClose    = mkFString "$fclose"
fsFFlush    = mkFString "$fflush"
fsTestPlusargs = mkFString "$test$plusargs"
fsRealToBits   = mkFString "$realtobits"
fsBitsToReal   = mkFString "$bitstoreal"

fsFile = mkFString "File"


-- | Classes hardcoded in the Prelude which were added for ContextErrors
fsBitwise, fsBitReduce, fsBitExtend, fsArith, fsOrd :: FString
fsBitwise          = mkFString "Bitwise"
fsBitReduce        = mkFString "BitReduction"
fsBitExtend        = mkFString "BitExtend"
fsArith            = mkFString "Arith"
fsOrd              = mkFString "Ord"

-- | Nice display names for instance hierarchy
fsLoop, fsBody :: FString
fsLoop = mkFString "Loop"
fsBody = mkFString "Body"

fsHide, fsHideAll, fsElements, fsvElements :: FString
fsHide             = mkFString "hide"
fsHideAll          = mkFString "hide_all"
fsElements         = mkFString "_elements"
fsvElements         = mkFString "_velements"
