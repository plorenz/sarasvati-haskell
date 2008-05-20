{-# OPTIONS -fglasgow-exts -cpp #-}
--    This file is part of Sarasvati.
--
--    Sarasvati is free software: you can redistribute it and/or modify
--    it under the terms of the GNU Lesser General Public License as
--    published by the Free Software Foundation, either version 3 of the
--    License, or (at your option) any later version.
--
--    Sarasvati is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU Lesser General Public License for more details.
--
--    You should have received a copy of the GNU Lesser General Public
--    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.
--
--    Copyright 2008 Paul Lorenz

module Workflow.GuardLang where
import qualified Workflow.EngineTypes as EngineTypes
import Data.Char
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.16

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

action_0 (8#) = happyShift action_2
action_0 (15#) = happyShift action_5
action_0 (16#) = happyShift action_6
action_0 (17#) = happyShift action_7
action_0 (4#) = happyGoto action_3
action_0 (7#) = happyGoto action_4
action_0 x = happyTcHack x happyFail

action_1 (8#) = happyShift action_2
action_1 x = happyTcHack x happyFail

action_2 (13#) = happyShift action_11
action_2 (14#) = happyShift action_12
action_2 (18#) = happyShift action_13
action_2 (5#) = happyGoto action_9
action_2 (6#) = happyGoto action_10
action_2 x = happyTcHack x happyFail

action_3 (20#) = happyAccept
action_3 x = happyTcHack x happyFail

action_4 x = happyTcHack x happyReduce_2

action_5 x = happyTcHack x happyReduce_9

action_6 x = happyTcHack x happyReduce_10

action_7 (14#) = happyShift action_8
action_7 x = happyTcHack x happyReduce_12

action_8 x = happyTcHack x happyReduce_11

action_9 (9#) = happyShift action_18
action_9 x = happyTcHack x happyFail

action_10 (11#) = happyShift action_16
action_10 (12#) = happyShift action_17
action_10 x = happyTcHack x happyReduce_6

action_11 (14#) = happyShift action_12
action_11 (18#) = happyShift action_13
action_11 (6#) = happyGoto action_15
action_11 x = happyTcHack x happyFail

action_12 x = happyTcHack x happyReduce_7

action_13 (13#) = happyShift action_11
action_13 (14#) = happyShift action_12
action_13 (18#) = happyShift action_13
action_13 (5#) = happyGoto action_14
action_13 (6#) = happyGoto action_10
action_13 x = happyTcHack x happyFail

action_14 (19#) = happyShift action_22
action_14 x = happyTcHack x happyFail

action_15 x = happyTcHack x happyReduce_5

action_16 (13#) = happyShift action_11
action_16 (14#) = happyShift action_12
action_16 (18#) = happyShift action_13
action_16 (5#) = happyGoto action_21
action_16 (6#) = happyGoto action_10
action_16 x = happyTcHack x happyFail

action_17 (13#) = happyShift action_11
action_17 (14#) = happyShift action_12
action_17 (18#) = happyShift action_13
action_17 (5#) = happyGoto action_20
action_17 (6#) = happyGoto action_10
action_17 x = happyTcHack x happyFail

action_18 (8#) = happyShift action_2
action_18 (15#) = happyShift action_5
action_18 (16#) = happyShift action_6
action_18 (17#) = happyShift action_7
action_18 (4#) = happyGoto action_19
action_18 (7#) = happyGoto action_4
action_18 x = happyTcHack x happyFail

action_19 (10#) = happyShift action_23
action_19 x = happyTcHack x happyFail

action_20 x = happyTcHack x happyReduce_3

action_21 x = happyTcHack x happyReduce_4

action_22 x = happyTcHack x happyReduce_8

action_23 (8#) = happyShift action_2
action_23 (15#) = happyShift action_5
action_23 (16#) = happyShift action_6
action_23 (17#) = happyShift action_7
action_23 (4#) = happyGoto action_24
action_23 (7#) = happyGoto action_4
action_23 x = happyTcHack x happyFail

action_24 x = happyTcHack x happyReduce_1

happyReduce_1 = happyReduce 6# 4# happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (StmtIF happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_1  4# happyReduction_2
happyReduction_2 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (StmtResult happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5# happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (ExprOR  happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5# happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (ExprAND happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  5# happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (ExprNOT happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6# happyReduction_7
happyReduction_7 (HappyTerminal (TokenSymbol happy_var_1))
	 =  HappyAbsSyn6
		 (ExprSymbol happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6# happyReduction_8
happyReduction_8 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  7# happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn7
		 (EngineTypes.AcceptToken
	)

happyReduce_10 = happySpecReduce_1  7# happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn7
		 (EngineTypes.DiscardToken
	)

happyReduce_11 = happySpecReduce_2  7# happyReduction_11
happyReduction_11 (HappyTerminal (TokenSymbol happy_var_2))
	_
	 =  HappyAbsSyn7
		 (EngineTypes.SkipNode happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7# happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn7
		 (EngineTypes.SkipNode []
	)

happyNewToken action sts stk [] =
	action 20# 20# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenIF -> cont 8#;
	TokenTHEN -> cont 9#;
	TokenELSE -> cont 10#;
	TokenAND -> cont 11#;
	TokenOR -> cont 12#;
	TokenNOT -> cont 13#;
	TokenSymbol happy_dollar_dollar -> cont 14#;
	TokenAccept -> cont 15#;
	TokenDiscard -> cont 16#;
	TokenSkip -> cont 17#;
	TokenLP -> cont 18#;
	TokenRP -> cont 19#;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [Token] -> HappyIdentity a
happyError' = HappyIdentity . parseError

evalGuard tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Stmt = StmtIF Expr Stmt Stmt
          | StmtResult EngineTypes.GuardResponse
  deriving Show

data Expr = ExprOR  Expr Expr
          | ExprAND Expr Expr
          | ExprSymbol String
          | ExprNOT Expr
  deriving Show

data Token = TokenIF
           | TokenTHEN
           | TokenELSE
           | TokenAND
           | TokenOR
           | TokenNOT
           | TokenSymbol String
           | TokenAccept
           | TokenDiscard
           | TokenSkip
           | TokenLP
           | TokenRP
  deriving Show

isLegalSymbolChar '.' = True
isLegalSymbolChar '_' = True
isLegalSymbolChar x   = isAlpha x || isDigit x

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexWord (c:cs)
lexer ('(':cs) = TokenLP : lexer cs
lexer (')':cs) = TokenRP : lexer cs

lexWord :: String -> [Token]
lexWord cs = case span isLegalSymbolChar cs of
                 ("IF",cs)      -> TokenIF          : lexer cs
                 ("THEN",cs)    -> TokenTHEN        : lexer cs
                 ("ELSE",cs)    -> TokenELSE        : lexer cs
                 ("AND",cs)     -> TokenAND         : lexer cs
                 ("OR",cs)      -> TokenOR          : lexer cs
                 ("NOT",cs)     -> TokenNOT         : lexer cs
                 ("Accept",cs)  -> TokenAccept      : lexer cs
                 ("Discard",cs) -> TokenDiscard     : lexer cs
                 ("Skip",cs)    -> TokenSkip        : lexer cs
                 (name,cs)      -> TokenSymbol name : lexer cs
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "GenericTemplate.hs" #-}








{-# LINE 49 "GenericTemplate.hs" #-}

{-# LINE 59 "GenericTemplate.hs" #-}

{-# LINE 68 "GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 1#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int# ->                    -- token number
         Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (1# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  1# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  1# tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action 1# 1# tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action 1# 1# tk (HappyState (action)) sts ( (HappyErrorToken (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
