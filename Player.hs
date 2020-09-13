-- | Write a report describing your design and strategy here.
-- STRATEGY
-- If the leading suit is hearts, then play the lowest hearts to avoid collecting points
-- If the leading suit is Spades, and there is a Queen of Spades within the trick, then
-- play the lowest Spade card in hand to avoid winning the Queen of Spades
-- If the player does not have any leading suit to play, then play the highest rank card to get the lead in the next trick
-- If the player does not have any leading suit to play, and the player has Queen of Spades in the hand, then
-- play the Queen of Spades to discard it from the hand
-- If the leading suit is not Hearts or Spades, then play the highest suit in the hand to get the lead for the next trick


module Player (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Cards
import Data.Maybe
import Data.List
import Hearts.Types

playCard :: PlayFunc
-- Empty trick list means beginning of trick. I have to lead.
playCard _ hand [] memory = leading hand memory
-- Trick list is not empty, so it means someone already played. I must not renege.
playCard _ hand trick _ = (reneging (suit $ fst $ last trick) hand trick, "")


-- | The lead function is for the start of the trick. It always tries to choose 2 of clubs if I have it.
leading :: [Card] -> Maybe ([(Card, PlayerId)], String) -> (Card,String)
leading hand memory = select (find (== (Card Club Two)) hand) where
    -- | Select the first card from hand if no particular card was provided. Does not satisfiy Bleeding rule.
    select :: Maybe Card -> (Card, String)
    -- if there is no two of clubs
    select Nothing 
        -- if hearts is forced to play for the first time
        -- * play the minimum card of hearts in hand
        | memory == Nothing && (filter (\x -> suit x /= Heart) hand) == [] = (findMinimumHand hand (head hand), "heartsPlayed")
        -- if hearts is already played then select any card except the queen of spades
        -- * play the maximum card
        | memoryString == "heartsPlayed" = (findMaximumHand filterSpadeQueenHand (head filterSpadeQueenHand), memoryString)
        -- if hearts is forced to play for the first time
        -- * play the minimum card of hearts in hand
        | memoryString == "" && (filter (\x -> suit x /= Heart) hand) == [] = (findMinimumHand hand (head hand), "heartsPlayed")
        -- otherwise filter out the hearts and play those cards which are not hearts
        | otherwise = (findMaximumHand filterHeartsHand (head filterHeartsHand), memoryString)
        where
            filterSpadeQueenHand = filter (\x -> x /= Card Spade Queen) hand
            memoryString = snd $ fromJust memory
            filterHeartsHand = filter (\x -> suit x /= Heart) hand
    select card 
     | memory == Nothing = (fromJust card, "")
     | otherwise = (fromJust card, snd $ fromJust memory)

-- | The renege function takes in a leader suit and selects the first occuring card of that suit from a list of cards.
-- Will throw an error if no cards of that suit is available. This renege is incomplete.
reneging :: Suit -> [Card] -> [(Card, PlayerId)]  -> Card
reneging leader hand trick
    -- if there is no leading suit to choose then just choose the first card (card of hearts) in the deck without queen in the hand
    -- play the lowest card
    | filter (\x -> suit x /= Heart) hand == [] = findMinimumHand filterQueen (head $ filterQueen)
-- if there is no leading suit and only hearts to play just choose the first card in the hand (forced to play hearts)
    | filter (\x -> suit x == leader) hand == [] = findMinimumHand filterHearts (head $ filterHearts)
-- if you have Queen of Spade in your hand and you are not leading then discard the Queen of Spade
    | filter (\x -> suit x == leader) hand == [] && (inHand (Card Spade Queen) hand) = Card Spade Queen 
    -- if the leader is Spade and a Queen of Spades is played then play the lowest Spade to avoid getting the Queen of Spades
    | (inTrick (Card Spade Queen) trick) && leader == Spade = findMinimumHand (filter (\x -> suit x == Spade) hand) (head $ (filter (\x -> suit x == Spade) hand))
-- otherwise choose the leading suit
-- play the highest card 
    | otherwise = findMaximumHand filterLeader (head $ filterLeader)
    where   
        filterLeader = filter (\x -> suit x == leader) hand
        filterHearts = filter (\x -> suit x /= Heart) hand
        filterQueen = filter (\x -> x /= Card Spade Queen) hand


-- function to find the minimum card in the hand
findMinimumHand :: [Card] -> Card -> Card
findMinimumHand [] _ = undefined
findMinimumHand [x] smallest = if (rank x) < (rank smallest) then x else smallest
findMinimumHand (y:xs) smallest = if (rank y) < (rank smallest) then 
                                            (findMinimumHand xs y) 
                                         else (findMinimumHand xs smallest) 

-- -- function to find the maximum card in the hand
findMaximumHand :: [Card] -> Card -> Card
findMaximumHand [] _ = undefined
findMaximumHand [x] largest = if (rank x) > (rank largest) then x else largest
findMaximumHand (y:xs) largest = if (rank y) > (rank largest) then 
                                           (findMaximumHand xs y) 
                                        else (findMaximumHand xs largest)


-- function to determine if the card is in the trick
inTrick :: Card -> [(Card, PlayerId)] -> Bool
inTrick _ [] = undefined
inTrick card [x] = fst x == card 
inTrick card (x:xs) = if fst x == card then True else (inTrick card xs)


-- -- function to determine if the card is in the hand
inHand :: Card -> [Card] -> Bool
inHand _ [] = undefined
inHand card [x] = card == x
inHand card (x:xs) = if card == x then True else (inHand card xs)

-- | Given a card, select its suit.
suit :: Card -> Suit
suit (Card s _) = s

-- Given a card, select its rank
rank :: Card -> Rank
rank (Card _ r) = r


-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
