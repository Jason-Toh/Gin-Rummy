-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import Rummy.Rules
import Data.List

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
-- type ActionFunc
--   = Card            -- ^ card on top of the discard pile
--   -> (Score, Score) -- ^ scores of (player, opponent) as of last round
--   -> Maybe String   -- ^ player's memory, on first player turn in the first round it will be Nothing
--   -> Maybe Draw     -- ^ opponent's chosen action, on first game turn it will be Nothing
--   -> [Card]         -- ^ the player's hand
--   -> (Draw, String) -- ^ which pile did the player chose to draw from and memory
pickCard :: ActionFunc
pickCard card (p1_score,p2_score) _ _ hand = (result,newMemory)
    where
        -- get the current melds
        melds = (makeMelds (p1_score,p2_score) "" hand)

        -- get the new melds with the current hand and the card at the discard pile
        newMelds = (makeMelds (p1_score,p2_score) "" (hand ++ [card]))

        -- save the current score in the memory
        newMemory = show (p1_score) ++ show (p2_score)
        
        -- if card can form melds with the current hand, take the card from the discard pile, else take from the stock pile
        result = if (length newMelds <= length melds) then Discard else Stock

-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
-- type PlayFunc
--   = Card              -- ^ picked card
--   -> (Score, Score)   -- ^ scores of (player, opponent) as of last round
--   -> String           -- ^ the player's memory
--   -> [Card]           -- ^ the player's hand (without new card)
--   -> (Action, String) -- ^ the player's chosen card and new memory
playCard :: PlayFunc
playCard card (p1_score,p2_score) memory hand = (action,newMemory)
    where
        -- player 1's score for the previous round
        old_p1_score = show (memory !! 0)
        -- player 2's score for the previous round
        old_p2_score = show (memory !! 1)
        -- update the memory with new score
        newMemory = show (p1_score) ++ show (p2_score) 

        -- gins or knocks are not allowed on the first round
        -- check if the scores changed, if they changed, it means it is first turn
        noGinOrKnock = (show (p1_score) /= old_p1_score) || (show (p2_score) /= old_p2_score)

        -- current meld without the new card
        melds = makeMelds (p1_score,p2_score) "" hand
        
         -- list of deadwoods in the current meld
        deadWoodList = [ d | d@(Deadwood _) <- melds ]

        -- check if there is a deadwood in the list
        ginFirstRound = length (deadWoodList) == 0 

        -- gets the highest deadwood value card to be discarded
        cardToBeDiscarded = meldToCard((getHighestDeadwoodCard deadWoodList)!!0)!!0

        -- remove the card to be discarded from the current hand
        newHand = removeDeadwoodCard (cardToBeDiscarded) hand

        -- make new melds with the picked cards
        newMelds = makeMelds (p1_score,p2_score) "" (newHand ++ [card])

        -- get the deadwoods of the new melds
        newDeadWoodList = [ d | d@(Deadwood _) <- newMelds]

        -- if there are no deadwood, call Gin
        gin = length (newDeadWoodList) == 0

        -- calculate the total deadwood score of the new melds
        deadWoodCount = foldr (+) 0 (map (cardPoints) newDeadWoodList)

        --check if the total deadwood is less than or equal to 10, if so call Knock
        knock = deadWoodCount <= 10

        -- if it is the first round, unable to call gin or knock
        action = if noGinOrKnock == True
                    -- if it a gin on the first round
                    then if ginFirstRound == True
                        -- if it is a gin, remove the highest deadwood value card from an existing meld (straights or sets)
                        then Action (Drop) (head (meldToCard (melds!!0)))
                        -- else, remove the highest deadwood value card
                        else Action (Drop) (cardToBeDiscarded)
                else
                    -- if there are no deadwoods, call Gin
                    if gin == True
                        then Action (Gin) (cardToBeDiscarded)
                    -- if the total deadwoods is less than <= 10, call Knock
                    else if knock == True
                        then Action (Knock) (cardToBeDiscarded)
                    -- else discard a card
                    else Action (Drop) (cardToBeDiscarded)

-- returns the highest deadwood card in the melds
getHighestDeadwoodCard :: [Meld] -> [Meld]
getHighestDeadwoodCard [] = []
getHighestDeadwoodCard [x] = [x]
getHighestDeadwoodCard (m1:m2:ms)
    | cardPoints m1 > cardPoints m2 = getHighestDeadwoodCard (m1:ms)
    | otherwise = getHighestDeadwoodCard (m2:ms)

-- remove the deadwood card from the hand
removeDeadwoodCard :: Card -> [Card] -> [Card]
removeDeadwoodCard _ [] = []
removeDeadwoodCard card (c:cs)
    | card == c = removeDeadwoodCard card cs
    | otherwise = c : removeDeadwoodCard card cs
    
-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
-- type MeldFunc
--   = (Score, Score) -- ^ scores of (player, opponent) as of last round
--   -> String        -- ^ the player's memory
--   -> [Card]        -- ^ cards in player's hand
--   -> [Meld]        -- ^ elected melds
makeMelds :: MeldFunc
makeMelds _ _ hand = finalMelds
    where
        -- generate all possible combinations in the hand
        allSequences = subsequences $ sort hand

        -- filter for straight 5 only as well as removing overlapping cards
        straight5List = removeDuplicate [x | x <- allSequences, fst(straightCheck x) == True, snd(straightCheck x) == 5]
        straight5List2 = [(Straight5 card1 card2 card3 card4 card5) | x <- straight5List, let (card1:card2:card3:card4:card5:_) = x]

        -- unqiue list are straight 5
        allCardsUnique = straight5List

        -- if the cards already appear in straight 5, then this operation will be omitted
        straight4List = if length straight5List == 0 
            -- filter for straight 4 only and ensure the the cards do not overlapped in allCardsUnique
            then removeDuplicate [x | x <- allSequences, fst(straightCheck x) == True, snd(straightCheck x) == 4, not $ isElem x (mconcat allCardsUnique)] else []
        straight4List2 = [(Straight4 card1 card2 card3 card4) | x <- straight4List, let (card1:card2:card3:card4:_) = x]

        -- new unique list with straight 5 and straight 4
        allCardsUnique2 = allCardsUnique ++ straight4List

        -- if the cards already appear in straight 5 and straight 4, then this operation will be omitted
        straight3List = if length straight5List == 0 && length straight4List == 0
            -- filter for straight 3 only and ensure the the cards do not overlapped in allCardsUnique2
            then removeDuplicate [x | x <- allSequences, fst(straightCheck x) == True, snd(straightCheck x) == 3, not $ isElem x (mconcat allCardsUnique2)] else []
        straight3List2 = [(Straight3 card1 card2 card3) | x <- straight3List, let (card1:card2:card3:_) = x]

        -- new unique list with straight 5, straight 4 and straight 3
        allCardsUnique3 = allCardsUnique2 ++ straight3List

        -- filter for set 4 only as well as removing overlapping cards and ensure the the cards do not overlapped in allCardsUnique3
        set4List = removeDuplicate [x | x <- allSequences, fst(setCheck x) == True, snd(setCheck x) == 4, not $ isElem x (mconcat allCardsUnique3)]
        set4List2 = [(Set4 card1 card2 card3 card4) | x <- set4List, let (card1:card2:card3:card4:_) = x]

        -- new unique list with straight 5, straight 4, straight 3 and set 4
        allCardsUnique4 = allCardsUnique3 ++ set4List

         -- if the cards already appear in set 4, then this operation will be omitted
        set3List = if length set4List == 0 
            -- filter for set 3 only as well as removing overlapping cards and ensure the the cards do not overlapped in allCardsUnique4
            then removeDuplicate [x | x <- allSequences, fst(setCheck x) == True, snd(setCheck x) == 3, not $ isElem x (mconcat allCardsUnique4)] else []
        set3List2 = [(Set3 card1 card2 card3) | x <- set3List, let (card1:card2:card3:_) = x]

        -- combine all the melds
        melds = straight5List2 ++ straight4List2 ++ straight3List2 ++ set4List2 ++ set3List2

        -- if there no straights or sets formed
        finalMelds = if length melds == 0 
            -- apply Deadwood using Functor fmap over the hand
            then Deadwood <$> hand 
            -- else add the remaining deadwood cards that do not appear in straights or sets
            else addDeadwood melds hand

-- Adds deadwood cards to the melds
addDeadwood :: [Meld] -> [Card] -> [Meld]
addDeadwood [] _ = []
addDeadwood melds [] = melds
addDeadwood melds (c:cs) 
    | c `elem` flattenList = addDeadwood melds cs 
    | otherwise = addDeadwood (melds ++ [Deadwood c]) cs
    where
        -- convert the melds [Meld] to a list of list of cards [[Card]] then flattens to a list of cards [Card]
        flattenList = mconcat $ map (meldToCard) melds

-- remove duplicates in the hand
removeDuplicate :: [[Card]] -> [[Card]]
removeDuplicate [] = []
removeDuplicate [x] = [x]
removeDuplicate (c1:c2:cs) | isElem c1 c2 = removeDuplicate (c1:cs)
                           | otherwise = c1 : c2 : removeDuplicate cs

-- checks if any elem in list1 appears in list2
isElem :: Eq a => [a] -> [a] -> Bool
-- combining Functors and Applicatives to use lifting where we check 
-- if there is an element that appears in another list
isElem list1 list2 = any (id) ((==) <$> list1 <*> list2)

-- converts a meld to a list of cards
meldToCard :: Meld -> [Card]
meldToCard (Deadwood card) = [card]
meldToCard (Set3 card1 card2 card3) = [card1, card2, card3]
meldToCard (Set4 card1 card2 card3 card4) = [card1, card2, card3, card4]
meldToCard (Straight3 card1 card2 card3) = [card1, card2, card3]
meldToCard (Straight4 card1 card2 card3 card4) = [card1, card2, card3, card4]
meldToCard (Straight5 card1 card2 card3 card4 card5) = [card1, card2, card3, card4, card5]

-- checks if the list of cards is a straight and return what straight it was
straightCheck :: [Card] -> (Bool,Int)
straightCheck cs 
    | (length cs == 3) = (check,3) -- return a straight 3
    | (length cs == 4) = (check,4) -- return a straight 4
    | (length cs == 5) = (check,5) -- return a straight 5
    | otherwise = (False,0)
    where
        -- This code was inspired from checkStraight in Rules.hs from line 77 to line 79
        check = (sameSuit cs) && (consecutive . ranked) cs
        ranked = sort . map getRank
        consecutive l = ((fromEnum . last) l - (fromEnum . head) l) == (length l - 1)

-- checks if the list of cards is a set and return what set it was
setCheck :: [Card] -> (Bool,Int)
setCheck cs 
    | (length cs == 3) = (check,3) -- returns a set 4
    | (length cs == 4) = (check,4) -- returns a set 3
    | otherwise = (False,0)
    where
        check = sameRank cs

-- checks if all the cards have the same rank
sameRank :: [Card] -> Bool
sameRank [] = False
sameRank ((Card _ rank):cs) = all(\x -> getRank x == rank) cs

-- checks if all the cards have the same suit
sameSuit :: [Card] -> Bool
sameSuit [] = False
sameSuit ((Card suit _):cs) = all(\x -> getSuit x == suit) cs

-- Get the suit of a card (Diamond, Club, Heart or Spade)
getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- Get the rank of a card (Ace,2,3,4,5,6,7,8,9,10,Jack,Queen or King)
getRank :: Card -> Rank
getRank (Card _ rank) = rank