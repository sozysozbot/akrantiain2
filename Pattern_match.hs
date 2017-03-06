data W = W String | Dollar_ 

type Condition = (String -> Bool)
type Rule = [Either Condition (String, W)]

sample1 = "sashimi"
sample2 = "stoxiet"


rule1 = [Right("t", W"t")]
rule2 = [Right("sh", W"ʃ")]
rule3 = [Right("s", W"z")]
rule4 = [Right("a",W"a")]
rule5 = [Right("i",W"i")]
rule6 = [Right("m",W"m")]
rule7 = [Right("s",W"s")]
rule8 = [Right("x",Dollar_), Right("i", W""), Right("e",Dollar_)]
rule9 = [Right("x", W"ʃ")]
rule10= [Right("e",W"e")]
rule11= [Right("s",W"s"), Left noVowel] 
rule12= [Right("o",W"o")]

noVowel :: Condition
noVowel ('a':_) = True
noVowel ('e':_) = True
noVowel ('i':_) = True
noVowel ('o':_) = True
noVowel ('u':_) = True
noVowel ('y':_) = True
noVowel _ = False

update :: Rule -> Stat -> Stat
update = undefined

data Stat = Stat
