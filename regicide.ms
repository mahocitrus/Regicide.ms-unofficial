readline = function(msg) // For grey hack and miniscript compatibility
    if globals.hasIndex("params") then return user_input(msg)
    return input(msg)
end function
if not string.hasIndex("trim") then
    string.trim = function(self)
        while self.len and {" ": 1, char(9): 1, char(10): 1, char(13): 1}.hasIndex(self[0])
            self = self[1:]
        end while
        while self.len and {" ": 1, char(9): 1, char(10): 1, char(13): 1}.hasIndex(self[-1])
            self = self[:-1]
        end while
        return self
    end function
end if
FisherYatesShuffle = function(a)
    n = len(a)
    i = n - 1
    while i > 0
        j = floor(rnd * (i + 1))
        tmp = @a[i]
        a[i] = @a[j]
        a[j] = @tmp
        i = i - 1
    end while
    return a
end function

buildTavern = function
    cards = []
    for s in "hdcs"
        cards.push((new Card).init("A", s))
        for r in range(2, 10)
            cards.push((new Card).init("" + r, s))
        end for
    end for
    return FisherYatesShuffle(cards)
end function
buildCastle = function
    castle = []
    for r in "KQJ"
        grp = []
        for s in "hdcs"
            grp.push((new Card).init(r, s))
        end for
        castle = castle + FisherYatesShuffle(grp)
    end for
    return castle
end function

int = function(n_str)
    n = val(n_str)
    if n != floor(n) then return print(n_str + " is not a integer!")
    if n + "" != n_str then return print(n_str + " is not a number!")
    return n
end function

parseIdx = function(raw)
    ret = []
    for i in raw.replace(",", " ").split(" ")
        i = int(i)
        if i == null then continue
        ret.push(i - 1)
    end for
    return ret
end function

Card = {"ranks": range(1, 10)}
Card.init = function(self, rank, suit)
    self.rank = rank
    self.suit = suit
    if rank == "A" then
        self.value = 1
    else if rank == "K" then
        self.value = 20
    else if rank == "Q" then
        self.value = 15
    else if rank == "J" then
        self.value = 10
    else
        self.value = val(self.rank)
    end if
    return self
end function
Card.name = function(self)
    if self.rank == "A" then return "Ace"
    if self.rank == "K" then return "King"
    if self.rank == "Q" then return "Queen"
    if self.rank == "J" then return "Jack"
    return self.rank
end function
Card.isAnimal = function(self)
    return self.rank == "A"
end function
Card.status = function(self)
    return self.rank + self.suit
end function

Enemy = new Card
Enemy.immune_label = {"h": "heal", "d": "draw", "c": "crit", "s": "shield"}
Enemy.init = function(self, card)
    super.init(card.rank, card.suit)
    self.card = card
    self.shield = 0
    self.damage_taken = 0
    return self
end function
Enemy.base_hp = function(self)
    return self.value * 2
end function
Enemy.hp = function(self)
    if self.base_hp - self.damage_taken >= 0 then return self.base_hp - self.damage_taken else return 0
end function
Enemy.atk = function(self)
    if self.value - self.shield >= 0 then return self.value - self.shield else return 0
end function
Enemy.defeated = function(self)
    return self.damage_taken >= self.base_hp
end function
Enemy.exact = function(self)
    return self.damage_taken == self.base_hp
end function
Enemy.add_shield = function(self, n)
    self.shield = self.shield + n
end function
Enemy.hit = function(self, n)
    self.damage_taken = self.damage_taken + n
    return n
end function
Enemy.status = function(self)
    return self.rank + self.suit + " " + self.name + char(10) + "HP:" + self.hp + "/" + self.base_hp + " Attack:" + self.atk + " Shield:" + self.shield + " [Immune:" + self.immune_label[self.suit] + "]"
end function

Game = {}
Game.init = function(self)
    self.hand_limit = 8
    self.tavern = buildTavern
    self.castle = buildCastle
    self.discard = []
    self.hand = []
    self.jokers = 2
    self.draw(self.hand_limit)
    self.next_enemy
    return self
end function
Game.help = function(self)
    helpMsg = "Regicide solo -- commands:" + char(10) +
    "  <n>          Play card n from hand" + char(10) +
    "  <n> <m> ...  Combo (same rank, value sum less than 10)" + char(10) +
    "  a<n>         Play Ace n alone" + char(10) +
    "  a<n> <m>     Pair Ace n with card m" + char(10) +
    "  yield        Skip attack and take damage (can not use if enemy atk is 0)" + char(10) +

    "  joker        Refresh hand (You have 2 Jokers at start)" + char(10) +
    "  ?            Show this help prompt" + char(10) +
    "  q            quit" + char(10) +
    "Suits: h=hearts d=diamonds c=clubs s=spades" + char(10) +
    "Powers (blocked if the enemy is the same suit):" + char(10) +
    "  h Heal  d Draw  c Crit(double-dmg)  s Shield" + char(10) +
    "Win tiers: 0 jokers used: gold, 1: silver, 2: bronze" + char(10) +
    "Learn how-to-play: https://www.regicidegame.com/site_files/33132/upload_files/RegicideRulesA4.pdf"
    return [0, print(helpMsg)]
end function
Game.sort_hand = function(self)
    hand = []
    set = {"c": [], "h": [], "s": [], "d": []}
    for card in self.hand
        set[card.suit].push(card)
    end for
    for suit in set.indexes
        set[suit].sort("value")
        hand = hand + set[suit]
    end for
    self.hand = hand
end function
Game.draw = function(self, n)
    i = 0
    while i < n and self.hand.len < self.hand_limit and self.tavern
        self.hand.push(self.tavern.pop)
        i = i + 1
    end while
    self.sort_hand
    print("Drew " + i + " cards! Hand:" + self.hand.len + "/" + self.hand_limit)
    return i
end function
Game.next_enemy = function(self)
    if not self.castle then return print("All enemies defeated!")
    top = self.castle.pop
    self.enemy = (new Enemy).init(top)
    self.played = []
    print("New enemy appeared!" + char(10) + self.enemy.status + char(10) + "Castle remaining: " + self.castle.len)
    return self.enemy
end function
Game.use_joker = function(self)
    if not self.jokers then return print("No more jokers left!")
    self.jokers = self.jokers - 1
    print("Using a joker. Discarded " + self.hand.len + " cards, redrawing " + self.hand_limit + " cards..." + char(10) + self.jokers + " Jokers left.")
    self.discard = self.discard + self.hand
    self.hand = []
    self.draw(self.hand_limit)
    return null
end function
Game.show = function(self)
    if not self.hand then return print("Hand empty!")
    row = []
    for i in self.hand.indexes
        row.push((i + 1) + "." + self.hand[i].status)
    end for
    return print("Hand: " + row.join("  "))
end function
Game.valid = function(self, idx)
    set = {}
    for i in idx
        if floor(i) != i or i < 0 or i >= self.hand.len then return print(" Bad index: " + (i + 1))
        set[i] = true
    end for
    if idx.len != set.len then return print(" Duplicated index!")
    return true
end function
Game.run = function(self)
    print("Welcome to Regicide. ?=help q=exit.")
    while true
        print("-- Tavern: " + self.tavern.len + "  Discard: " + self.discard.len + "  Jokers: " + self.jokers + char(10) + self.enemy.status)
        self.show
        if not self.hand and not self.jokers and self.enemy.atk <= 0 then
            print("Secret Ending: Realizing that neither you nor the foe before you can harm the other, you battle with your own thoughts before turning your back on the castle. It's a defeat, but you live to tell the tale. (Enemy ATK is 0 and no valid moves available.)")
            return 2
        end if
        state = self.step1
        play = state[1]
        state = state[0]
        if state then return state
        if play == null then continue
        if play == "yield" then
            state = self.step4
            if state then return state
            continue
        end if
        self.apply_suit_powers(play)
        self.apply_damage(play)
        self.played = self.played + play
        if self.enemy.defeated then
            state = self.resolve
            if state then return state
            continue
        end if
        state = self.step4
        if state then return state
    end while
end function
Game.step1 = function(self)
    raw = readline("> ").trim.lower
    if not raw then return [0, null]
    if {"q": 1, "quit": 1, "exit": 1}.hasIndex(raw) then return [3, null]
    if raw == "?" or raw == "help" then return self.help
    if raw == "yield" then
        if self.enemy.atk == 0 then return [0, print("  Can't yield -- Enemy ATK is 0 (no damage to absorb)")]
        print("  yield.")
        return [0, "yield"]
    end if
    if raw == "joker" then return [0, self.use_joker]
    if raw.indexOf("a") == 0 then return [0, self.parse_animal(raw)]
    idx = parseIdx(raw)
    if not idx then return [0, print("  type ? for help.")]
    if not self.valid(idx) then return [0, null]
    cards = []
    for i in idx
        cards.push(self.hand[i])
    end for
    if cards.len > 1 then
        set = {"sum":0, "ranks": {}}
        for c in cards
            if c.isAnimal then return [0, print("  Aces can't combo -- use 'a<n> <m>' to pair")]
            set.ranks[c.rank] = true
            set.sum = set.sum + c.value
        end for
        if len(set.ranks) > 1 then return [0, print("  Combo must be the same rank")]
        if set.sum > 10 then return [0, print("  combo total " + set.sum + " > 10")]
    end if
    for i in idx.sort(null, 0)
        self.hand.remove(i)
    end for
    return [0, cards]
end function
Game.parse_animal = function(self, raw)
    parts = raw[1:].replace(",", " ").split(" ")
    if not parts or parts.len > 2 then return print("  usage: a<n>  or  a<n> <m>")
    ai = int(parts[0])
    if not ai then return print("  Bad Ace index")
    ai = ai - 1
    if (not (ai >= 0 and ai < self.hand.len)) or (not self.hand[ai].isAnimal) then return print("  " + (ai + 1) + " is not an Ace")
    if parts.len == 1 then
        cards = [self.hand[ai]]
        self.hand.remove[ai]
        return card
    end if
    bi = int(parts[1])
    if not bi then return print("  Bad second index")
    bi = bi - 1
    if bi == ai then return print("  Same index")
    if not (bi >= 0 and bi < self.hand.len) then return print("  index " + (bi + 1) + " out of range")
    idx = [ai, bi].sort
    cards = [self.hand[ai], self.hand[bi]]
    self.hand.remove(idx[1])
    self.hand.remove(idx[0])
    return cards
end function
Game.apply_suit_powers = function(self, cards)
    e = self.enemy
    set = {"sum": 0, "suits": {}}
    for c in cards
        set.sum = set.sum + c.value
        set.suits[c.suit] = true
    end for
    if set.suits.hasIndex("h") then
        if e.suit == "h" then
            print("  Heal suppressed! (Immune)")
        else if not self.discard then
            print("  Discard empty! Nothing to heal!")
        else
            FisherYatesShuffle(self.discard)
            moved = self.discard[:set.sum]
            self.discard = self.discard[set.sum:]
            self.tavern = moved + self.tavern
            print("  Heal: moved " + moved.len + " cards to the bottom of tavern")
        end if
    end if
    if set.suits.hasIndex("d") then
        if e.suit == "d" then
            print("  Draw suppressed! (Immune)")
        else
            print("  Draw: draw " + set.sum + " cards")
            self.draw(set.sum)
        end if
    end if
    if set.suits.hasIndex("s") then
        if e.suit == "s" then
            print("  Shield suppressed! (Immune)")
        else
            e.add_shield(set.sum)
            print("  Shield: shield +" + set.sum + " -> Enemy atk is now " + e.atk)
        end if
    end if
end function
Game.apply_damage = function(self, cards)
    e = self.enemy
    set = {"sum": 0, "suits": {}}
    for c in cards
        set.sum = set.sum + c.value
        set.suits[c.suit] = true
    end for
    if set.suits.hasIndex("c") then
        if e.suit == "c" then
            print("  Crit suppressed! (Immune)")
        else
            print("  Crit: deal double damage -> " + set.sum + "x2=" + (set.sum * 2))
            set.sum = set.sum * 2
        end if
    end if
    e.hit(set.sum)
    print("  Hit " + set.sum + "!  Enemy HP: " + e.hp + "/" + e.base_hp)
end function
Game.resolve = function(self)
    e = self.enemy
    defeated_card = e.card
    if e.exact then
        self.tavern.push(defeated_card)
        print("  Exact kill! " + defeated_card.status + " moved to top of tavern!")
    else
        self.discard.push(defeated_card)
        print("  Moved " + defeated_card.status + " to discard.")
    end if
    self.discard = self.discard + self.played
    print("  Defeated " + e.name + "!")
    if not self.castle then
        print("All enemies defeated! You won!")
        return 1
    end if
    self.next_enemy
    return 0
end function
Game.step4 = function(self)
    dmg = self.enemy.atk
    if dmg == 0 then return not (not print("  Enemy attack fully shielded!"))
    print("  Enemy hits " + dmg + " damage, discard card with value sum larger or equal.")
    remaining = dmg
    discarded = []
    while remaining > 0
        if not self.hand and not self.jokers then
            self.discard = self.discard + discarded
            print("  Can't cover damage. You lost!")
            return 2
        end if
        self.show
        if self.jokers then suffix = " (or 'joker')" else suffix = ""
        print("  Need to discard " + remaining + " more value" + suffix + ".")
        raw = readline("> ").trim.lower
        if raw == "joker" then
            if not self.jokers then
                print("  No jokers left!")
                continue
            end if
            self.discard = self.discard + self.discarded
            discarded = []
            self.use_joker
            continue
        end if
        if not self.hand then
            print("  Hand empty! type 'joker'!")
            continue
        end if
        idx = int(raw)
        if not idx then continue
        idx = idx - 1
        if not self.valid([idx]) then continue
        chosen = self.hand[idx]
        self.hand.remove(idx)
        remaining = remaining - chosen.value
        discarded.push(chosen)
        print("  Discarded " + chosen.status + ", worth " + chosen.value + ", still need to discard " + ((remaining >= 0) * remaining))
    end while
    self.discard = self.discard + discarded
    return 0
end function

while true
    game = (new Game).init
    state = game.run
    medals = ["Bronze", "Silver", "Gold"]
    if state == 1 then
        print(medals[game.jokers] + " victory! You defeated all bosses and saved the country!")
    else if state == 2 then
        print("Game over.")
    end if
    if readline("Start a new game? (y/N) ").trim.lower != "y" then break
end while