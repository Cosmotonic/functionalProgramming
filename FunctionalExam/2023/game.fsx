
type Mission = MissionElemenets list
and MissionElemenets = 
    MissionTitle of string
    | MissionRequirements of string * Mission 
let RecklessPeteTitle = MissionTitle "The hunt for Reckless Pete!"
let RuthlessJohnTitle = MissionTitle "Where is Ruthless John?"
let hangmansFedTitle = MissionTitle "Keeping death busy"
let fasterHovesTitle = MissionTitle "Find yourself a horse!"
let happyHangmanTitle = MissionTitle "Find yourself a horse!"
let happyHangmanRequirement = MissionRequirements ("Happy Hangman Prerequisits missions", [RuthlessJohnTitle; RecklessPeteTitle])
let missionLists = MissionRequirements ("All Missions", [happyHangmanRequirement; fasterHovesTitle])
let missionBoard = [missionLists]

// Get all mission titles: 
let rec findAllMissions missionsBoard = 
    match missionsBoard with 
    | [] -> " "
    | h::t -> match h with 
                | MissionTitle h -> h + findAllMissions t
                | MissionRequirements (name, innerMissions) ->  findAllMissions innerMissions + findAllMissions t

findAllMissions missionBoard

// Get all mission titles in a list: 
let rec ListAllMissions missionsBoard = 
    match missionsBoard with 
    | [] -> []
    | h::t -> match h with 
                | MissionTitle h -> [h] @ ListAllMissions t
                | MissionRequirements (name, innerMissions) ->  ListAllMissions innerMissions @ ListAllMissions t

ListAllMissions missionBoard

// fold all missions.  // https://www.youtube.com/watch?v=ucYQyIq6UOM
let foldMissions missionsBoard = 
    let listMis = ListAllMissions missionBoard
    List.fold (fun s i -> i+s) " " listMis

foldMissions missionBoard

// count all missions. 
let lengthMissions missionsBoard = 
    let listMis = ListAllMissions missionBoard
    List.length listMis

lengthMissions missionBoard

let countMissions missionBoard = 
    let listMis = ListAllMissions missionBoard
    
    let rec counterForMissions NameList = 
        match NameList with 
        | [] -> 0
        | h::t -> 1 + counterForMissions t 

    counterForMissions listMis
    
countMissions missionBoard

let countMissionsAcc missionBoard = 
    let listMis = ListAllMissions missionBoard
    
    let rec counterForMissions NameList acc = 
        match NameList with 
        | [] -> acc
        | h::t ->  counterForMissions t (acc+1)
    counterForMissions listMis 0
countMissionsAcc missionBoard



