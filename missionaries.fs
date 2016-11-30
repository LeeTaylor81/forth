variable usedcounter
variable canindex
variable crindex
variable used 100 cells allot
variable candidatestack 100 cells allot
variable crumbstack 100 cells allot

: 3dup ( x y z -- x y z x y z)
   dup
   2over
   rot 
;

: 3drop ( x y z -- )
   drop
   2drop 
;

: pack ( near m c -- packedstate ) 
   swap
   10 * +
   swap
   100 * + 
;

: unpack ( packedstate -- near m c )
   dup
   10 mod  
   swap    
   dup
   100 /   
   swap    
   100 mod 
   10 /    
   rot     
;

: printstate ( near m c -- )
	swap
  	rot
	." [ " 2 = if ." near " else ." far  " then . . ." ]"
;

: isused ( n -- bool )
    \ assume false result
    0            ( n false )
    swap            ( false n )
    \ loop through all the elements
    usedcounter         ( false n &usedcounter ) 
    @            ( false n usedcounter )
    0            ( false n usedcounter 0 )
    do            ( false n )
        \ compare n with elt i
        dup        ( false n n )
        used        ( false n n &used )
        i        ( false n n &used i )
        cells        ( false n n &used i *8 )
        +        ( false n n &used[i] )
        @        ( false n n used[i] )
        =        ( false n n==used[i] )
        if        ( false n )
          nip        ( n )
          -1        ( n true )
          swap        ( true n )
          leave    ( true n )
        then
    loop            ( bool n )
    drop            ( bool )
;

: addused ( n -- )
      used usedcounter @ cells + ! ( stores value )
    1 usedcounter +!
;

: pushcandidate ( n -- )
    candidatestack canindex @ cells + !
    1 canindex +!
;

: popcandidate ( -- n )
    -1 canindex +!
    candidatestack canindex @ cells + @
;

: pushcrumb ( n -- )
    crumbstack crindex @ cells + !
    1 crindex +!
;

: popcrumb ( -- n )
    -1 crindex +!
    crumbstack crindex @ cells + @
;

: dump ( addr cell-count -- )
	0 do dup i cells + @ unpack printstate cr loop drop
;

: printused ( -- )
    usedcounter @ 0 = if ." empty"
    else used usedcounter @ dump
    then
;

: printcandidates ( -- )
    canindex @ 0 = if ." empty"
    else candidatestack canindex @ dump
    then
;

: printcrumbs ( -- )
    crindex @ 0 = if ." empty"
    else crumbstack crindex @ dump
    then
;

: startstate ( -- near m c )
    2 3 3
;

: isgoal ( near m c -- bool )
    pack
    300 =
;

: isvalid ( near m c -- bool )
    dup
    4 <
    over
    -1 >
    and 
    ( near m c B )
    rot 
    2swap
    rot
    ( B near c m )
    dup
    4 <
    over
    -1 >
    and
    ( B near c m B2 )
    swap
    2swap
    swap
    ( B B2 m c near )
    dup
    4 <
    over
    1 >
    and
    2rot
    ( m c near B3 B B2 )
    and
    and
    ( m c near B )
    rot 2swap rot rot
    ( B near c m )
    dup
    0 =
	over
	3 =
    2swap
	=
    or or swap drop 
    ( B B ) 
    and
    ( B )
    
        
;

: addcandidate ( near m c -- )
    ( RETURN STACK )
    pack
    dup
    unpack
    isvalid
    if dup isused
        if ." repeat  " unpack printstate cr
            else ." fresh   " dup unpack printstate cr 
                dup addused pushcandidate
    then
    else ." invalid " unpack printstate cr
    then
;
:  moveboatfar
    rot 1 + rot rot
;
:  moveboatnear
    rot 1 - rot rot
;
: movecannibalfar
    3dup
    1 -
    moveboatfar
    addcandidate
;
: movecannibalnear
    3dup
    1 +
    moveboatnear
    addcandidate
;
: movetwocannibalsfar
    3dup
    2 -
    moveboatfar
    addcandidate
;
: movetwocannibalsnear
    3dup
    2 +
    moveboatnear
    addcandidate
;
: movemissionaryfar
    3dup
    swap 1 - swap
    moveboatfar
    addcandidate
;
: movemissionarynear
    3dup
    swap 1 + swap
    moveboatnear
    addcandidate
;
: movemissionariesfar
    3dup
    swap 2 - swap
    moveboatfar
    addcandidate
;
: movemissionariesnear
    3dup
    swap 2 + swap
    moveboatnear
    addcandidate
;
: moveoneofeachfar
	3dup
    1 -
    swap 1 - swap
    moveboatfar
    addcandidate
;
: moveoneofeachnear
	3dup
    1 +
    swap 1 + swap
    moveboatnear
    addcandidate
;

: successors ( near m c -- )
    rot dup
    2 = 
    if
    rot rot 
    movecannibalfar
    movetwocannibalsfar
    movemissionaryfar
    movemissionariesfar
    moveoneofeachfar
    else
    rot rot    
    movemissionarynear
    movecannibalnear
    movemissionariesnear
    movetwocannibalsnear
    moveoneofeachnear
    then 3drop
;

: search ( -- )
    popcandidate
    dup
    pushcrumb
    unpack
    3dup
    isgoal
    if ." Here is the Solution, Russ " cr  cr printcrumbs
        else ." candidates:" cr successors cr recurse
        then
    popcrumb
;

: start
	cr
    0 canindex !
    0 crindex !
    0 usedcounter !
    startstate
    pack
    dup
    pushcandidate
    addused
    search
;

