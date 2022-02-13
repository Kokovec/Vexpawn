;***************************************************************************
; Vectrex Hexpawn (by Dan Siewers)
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;***************************************************************************
;***************************************************************************
; DEFINE SECTION
;***************************************************************************
; load vectrex bios routine definitions
                    INCLUDE  "VECTREX.I"                  ; vectrex function includes
;***************************************************************************
; Variable / RAM SECTION
;***************************************************************************
; insert your variables (RAM usage) in the BSS section
; user RAM starts at $c880
                    BSS      
                    ORG      $c880                        ; start of our ram space 
utility_byte:       DS       1 
test_word           DS       2 
players_left_flag   DS       1                            ; 00 = players left, 01 = no players left 
playing_music_flag  DS       1                            ; 00= not playing music, 01= playing music 
start_flag          DS       1                            ; 0=start new game, 1= keep data for next game 
box_selector_mode   DS       1                            ; 00=don't show, 01=show 
box_orientation     DS       1                            ; 00=bottom, 01=top 
box_position        DS       1                            ; follows box number layout 
joy_prev_state_x    DS       1                            ; previous joy state: 0=not moved, 1=moved 
joy_prev_state_y    DS       1                            ; previous joy state: 0=not moved, 1=moved 
button_prev_state_4  DS      1                            ; previous state of button4: 0=not moved, 1=moved 
turn_flag           DS       1                            ;Who's turn is it: 0=Vectrex, 1=Player 
player_from         DS       1                            ; holds player 'From' info 
player_to           DS       1                            ; holds player 'To' info 
player_from_to_flag  DS      1                            ; from/to flag: 0=from, 1=To 
blink_state         DS       1                            ;blink state: 0=no draw, 1=draw 
board_state_count   DS       1                            ; counter for determining board state (0-33) 
vec_pawn_blink      DS       1                            ; 0= no blink, 1 = blink 
vec_pawn_blink_count  DS     1 
move_count          DS       1                            ; move tick counter 
Vec_move_before_last_address  DS  2                       ; Vec before last move address (pointer) 
Vec_last_move_address  DS    2                            ; Vec last move address (pointer) 
ym_len:             ds       2                            ; length of music table 
ym_reg_base:        ds       2 
ym_data_pos:        ds       2 
Play_Board_Status:  DS       9                            ; status of pawn locations, follows board numbering 
Play_Board_Blink    DS       9                            ; pawn blinking: 0=no blink, 1=blink 
Shadow_ML_Model     DS       264 
;***************************************************************************
; HEADER SECTION
;***************************************************************************
; The cartridge ROM starts at address 0
                    CODE     
                    ORG      0 
; the first few bytes are mandatory, otherwise the BIOS will not load
; the ROM file, and will start MineStorm instead
                    DB       "g GCE 2022", $80 ; 'g' is copyright sign
                    DW       music1                       ; music from the rom 
                    DB       $F8, $50, $40, -$40          ; hight, width, rel y, rel x (from 0,0) 
                    DB       "VEXPAWN", $80               ; some game information, ending with $80
                    DB       0                            ; end of game header 
;***************************************************************************
; CODE SECTION
;***************************************************************************
; here the cartridge program starts off
init: 
                    jsr      Read_Btns                    ; set initial button state 
                    LDA      #1                           ; these set up the joystick 
                    STA      Vec_Joy_Mux_1_X              ; enquiries 
                    LDA      #3                           ; allowing only all directions 
                    STA      Vec_Joy_Mux_1_Y              ; for joystick one 
                    LDA      #0                           ; this setting up saves a few 
                    STA      Vec_Joy_Mux_2_X              ; hundred cycles 
                    STA      Vec_Joy_Mux_2_Y              ; don't miss it, if you don't 
                                                          ; need the second joystick! 
                    ldd      #296                         ; copy all learning data to RAM 
                    std      test_word 
                    ldx      #ML_Model 
                    ldy      #Shadow_ML_Model 
ml_model_loop: 
                    lda      ,x+ 
                    sta      ,y+ 
                    ldd      test_word 
                    subd     #1 
                    std      test_word 
                    bne      ml_model_loop 
                    lda      #0 
                    sta      start_flag                   ; new game 
                    sta      joy_prev_state_y             ; reset joy y state flag 
                    sta      button_prev_state_4          ; reset button 4 pressed flag 
                    sta      playing_music_flag           ; clear playing music flag 
intro: 
                    ldu      #SONG_DATA                   ; init music 
                    jsr      init_ym_sound 
intro_loop: 
                    jsr      Wait_Recal 
                    jsr      Intensity_5F 
                    ldu      #intro_text1                 ; show intro text 
                    lda      #100 
                    ldb      #-50 
                    jsr      Print_Str_d 
                    ldu      #intro_text2 
                    lda      #50 
                    ldb      #-80 
                    jsr      Print_Str_d 
                    lda      start_flag                   ; highlight or lowlight? 
                    beq      high_intensity_3             ; highlight 
                    jsr      Intensity_5F                 ; lowlight 
                    bra      text_3 

high_intensity_3: 
                    jsr      Intensity_7F 
text_3: 
                    ldu      #intro_text3 
                    lda      #30 
                    ldb      #-80 
                    jsr      Print_Str_d 
                    lda      start_flag                   ; highlight or lowlight? 
                    beq      low_intensity_4              ; lowlight 
                    jsr      Intensity_7F                 ; highlight 
                    bra      text_4 

low_intensity_4: 
                    jsr      Intensity_5F 
text_4: 
                    ldu      #intro_text4 
                    lda      #10 
                    ldb      #-80 
                    jsr      Print_Str_d 
                    jsr      do_ym_sound                  ; go make some noise 
                    JSR      Joy_Digital                  ; read joystick positions 
                    LDA      Vec_Joy_1_Y                  ; load joystick 1 position 
                    beq      no_joy_y                     ; if zero, than no y position 
                    bmi      y_down                       ; if negative, then down 
                    lda      joy_prev_state_y             ; still being pressed from last check? 
                    bne      keep_going                   ; yes, ignore 
                    lda      #0                           ; nope.. 
                    sta      start_flag                   ; set start flag to New Game (fresh data) 
                    sta      joy_prev_state_y             ; flag joy as being pressed 
                    bra      keep_going 

y_down: 
                    lda      joy_prev_state_y             ; still being pressed from last check? 
                    bne      no_joy_y                     ; yes, ignore 
                    lda      #1 
                    sta      start_flag                   ; set start flag to New Game (fresh data) 
                    sta      joy_prev_state_y             ; flag joy as being pressed 
                    bra      keep_going 

no_joy_y: 
                    clr      joy_prev_state_y             ; clear the joy state flag 
keep_going: 
                    jsr      Read_Btns                    ; get button status 
                    bita     #$08                         ; test for button 1-4 
                    beq      button_not_pressed           ; not pressed 
                    jsr      Clear_Sound 
                    lda      #1                           ; flag that y button was pressed 
                    sta      button_prev_state_4 
                    clr      playing_music_flag           ; flag that no music is playing 
                    lda      start_flag 
                    beq      init_fresh 
                    bra      init_new 

button_not_pressed: 
                    ldd      ym_len                       ; check if end of music 
                    lbeq     intro                        ; yep, start music over 
                    lbra     intro_loop                   ; no, keep playing music next round 

init_fresh: 
                    ldd      #296 
                    std      test_word 
                    ldx      #ML_Model 
                    ldy      #Shadow_ML_Model 
init_ml_model_loop: 
                    lda      ,x+ 
                    sta      ,y+ 
                    ldd      test_word 
                    subd     #1 
                    std      test_word 
                    bne      init_ml_model_loop 
init_new: 
                    lda      #255 
                    sta      move_count                   ; reset move tick counter 
                    lda      #1 
                    sta      box_selector_mode            ;set selector to show the box 
                    lda      #0 
                    sta      board_state_count            ; reset board state counter 
                    sta      box_orientation              ; box starts at bottom 
                    sta      player_from                  ; init player box selection flags 
                    sta      player_to 
                    sta      player_from_to_flag          ; init player from/to flag (to from) 
                    sta      joy_prev_state_x             ; reset joy_x sate 
                    sta      joy_prev_state_y             ; reset joy_x state 
                    sta      blink_state                  ; no pawns blinking 
                    sta      vec_pawn_blink               ; 
                    lda      #8 
                    sta      box_position                 ; set box position to 8 (bottom right) 
                    lda      #1                           ; player makes first move 
                    sta      turn_flag 
                    ldb      #9                           ; initialize pawn blink position 
                    lda      #0 
                    ldx      #Play_Board_Blink 
init_blink_loop: 
                    sta      ,x+ 
                    decb     
                    bne      init_blink_loop 
                    ldb      #9                           ; initialize pawn positions 
                    ldx      #pawn_start_positions 
                    ldy      #Play_Board_Status 
init_pawns_loop: 
                    lda      ,x+ 
                    sta      ,y+ 
                    decb     
                    bne      init_pawns_loop 
main: 
                    jsr      draw_board_and_box 
                    lda      turn_flag 
                    lbeq     Vec_turn 
                    JSR      Joy_Digital                  ; read joystick positions 
                    LDA      Vec_Joy_1_X                  ; load joystick 1 position 
                    BEQ      no_x_movement                ; if zero, than no x position 
                    BMI      left_move                    ; if negative, then left 
right_move: 
                    lda      joy_prev_state_x             ; joy still being pressed from last check? 
                    bne      x_done                       ; yes, ignore until it has been released 
                    lda      box_position                 ; get position of selector box 
                    cmpa     #2                           ; check if at one of the boundaries 
                    beq      x_done 
                    cmpa     #5 
                    beq      x_done 
                    cmpa     #8 
                    beq      x_done 
                    inc      box_position                 ; move selector box to the right 
                    inc      joy_prev_state_x             ; flag that joy was pressed 
                    BRA      x_done                       ; goto x done 

left_move: 
                    lda      joy_prev_state_x             ; joy still being pressed from last check? 
                    bne      x_done                       ; yes, ignore until it has been released 
                    lda      box_position                 ; get position of selector box 
                    cmpa     #0                           ; check if at one of the boundaries 
                    beq      x_done 
                    cmpa     #3 
                    beq      x_done 
                    cmpa     #6 
                    beq      x_done 
                    dec      box_position                 ; move selector box to the left 
                    inc      joy_prev_state_x             ; flag that joy was pressed 
                    BRA      x_done                       ; goto x done 

no_x_movement: 
                    clr      joy_prev_state_x             ; clear the joy state flag 
x_done: 
                    LDA      Vec_Joy_1_Y                  ; load joystick 1 position 
                    BEQ      no_y_movement                ; if zero, than no y position 
                    BMI      down_move                    ; if negative, then down 
up_move: 
                    lda      joy_prev_state_y             ; joy still being pressed from last check? 
                    bne      y_done                       ; yes, ignore until it has been released 
                    lda      box_position                 ; get position of selector box 
                    cmpa     #0                           ; check if at one of the boundaries 
                    beq      y_done 
                    cmpa     #1 
                    beq      y_done 
                    cmpa     #2 
                    beq      y_done 
                    dec      box_position                 ; move selector box up 
                    dec      box_position 
                    dec      box_position 
                    inc      joy_prev_state_y             ; flag that joy was pressed 
                    BRA      y_done                       ; goto y done 

down_move: 
                    lda      joy_prev_state_y             ; joy still being pressed from last check? 
                    bne      y_done                       ; yes, ignore until it has been released 
                    lda      box_position                 ; get position of selector box 
                    cmpa     #6                           ; check if at one of the boundaries 
                    beq      y_done 
                    cmpa     #7 
                    beq      y_done 
                    cmpa     #8 
                    beq      y_done 
                    inc      box_position                 ; move selector box down 
                    inc      box_position 
                    inc      box_position 
                    inc      joy_prev_state_y             ; flag that joy was pressed 
                    BRA      y_done                       ; goto y done 

no_y_movement: 
                    clr      joy_prev_state_y             ; clear the joy state flag 
y_done: 
                    JSR      Read_Btns                    ; get button status 
                    bita     #$08                         ; test for button 1-4 
                    LBEQ     non_button                   ; not pressed 
                    lda      button_prev_state_4          ; button 4 still being pressed? 
                    lbne     non_button                   ; yes, ignore 
button_pressed: 
                    lda      #1                           ; flag that y button was pressed 
                    sta      button_prev_state_4 
                    lda      player_from_to_flag          ; check if selecting 'From' 
                    bne      player_to_handle             ; nope, player selecting 'To' 
                    lda      box_position                 ; get current selector position 
                    ldx      #Play_Board_Status           ; check if player has a pawn in that position 
                    ldb      a,x 
                    cmpb     #01 
                    lbne     non_button                   ; nope, ignore 
                    ldb      #-1 
                    stb      Vec_Expl_Flag 
                    sta      player_from                  ; yes, save 'From' pawn position 
                    ldx      #Play_Board_Blink 
                    ldb      #1 
                    stb      a,x                          ; save it to blink that pawn 
                    inc      player_from_to_flag          ; flag for 'To' selection next go-round 
                    lbra     main 

player_to_handle: 
                    ldb      player_from                  ; get player "From" pawn 
                    decb                                  ; calculate box above it 
                    decb     
                    decb     
                    cmpb     box_position                 ; check if legal 'Up' move 
                    bne      player_check_capture         ; nope, check if trying to capture 
                    lda      box_position                 ;check to see if pawn in move position 
                    ldx      #Play_Board_Status 
                    ldb      a,x 
                    beq      player_to_handle_legal       ; new position is empty, move the pawn 'Up' 
                    bra      player_not_legal_move        ; yes, not a legal move, start over 

player_check_capture: 
                    lda      box_position                 ;check to see if Vec pawn in capture position 
                    ldx      #Play_Board_Status 
                    ldb      a,x 
                    cmpb     #02                          ; is Vec pawn there? 
                    bne      player_not_legal_move        ; nope, start over 
                    lda      #2                           ; check for legal capture move 
                    sta      utility_byte 
                    ldx      #player_captures             ; point to player legal capture table 
                    ldb      player_from                  ; get original player pawn position 
                    lslb                                  ; table offset 
player_capture_loop 
                    lda      b,x                          ; get the legal move value from table 
                    cmpa     box_position                 ; is it a legal capture? 
                    beq      player_to_handle_legal       ; yes, go do it 
                    leax     1,x                          ; no, try next move in table 
                    dec      utility_byte 
                    bne      player_capture_loop          ; not a legal capture, start over 
player_not_legal_move: 
                    clr      player_from_to_flag          ; reset player selector to 'From' 
                    lda      player_from 
                    ldx      #Play_Board_Blink 
                    ldb      #0                           ;clear 'From" pawn blinking state 
                    stb      a,x                          ; save it 
                    lbra     main 

player_to_handle_legal: 
                    ldb      #-1 
                    stb      Vec_Expl_Flag 
                    lda      player_from 
                    ldb      #0                           ;clear pawn 'From' position 
                    ldx      #Play_Board_Status 
                    stb      a,x 
                    lda      box_position                 ; set new pawn position 
                    ldb      #1 
                    stb      a,x 
                    clr      player_from_to_flag 
                    lda      box_position                 ; check if player wins 
                    cmpa     #2 
                    ble      player_wins 
                    clr      turn_flag                    ; Vectrex gets next move 
                    clr      box_selector_mode            ; turn off selector box 
                    lbra     main 

player_wins: 
                    ldx      Vec_last_move_address        ; get last move ML shadow address 
                    lda      #255                         ; delete it 
                    sta      ,x+ 
                    sta      ,x 
                    ldu      #SONG_DARTS_DATA             ; init music 
                    jsr      init_ym_sound 
                    jsr      Intensity_5F 
player_wins_loop: 
                    jsr      Reset0Ref 
                    ldu      #player_wins_text            ; show intro text 
                    lda      #100 
                    ldb      #-65 
                    jsr      Print_Str_d 
                    jsr      draw_board_and_box 
                    jsr      do_ym_sound                  ; go make some noise 
                    ldd      ym_len                       ; check if end of music 
                    lbeq     intro                        ; yep, start music over 
                    bra      player_wins_loop             ; no, keep playing music next round 

non_button: 
                    lda      #0                           ; clear the button state (not pressed) 
                    sta      button_prev_state_4 
                    lbra     main 

Vec_turn: 
                    lda      board_state_count            ; get current count value (board_state_pointer position) 
                    cmpa     #37                          ; reached the end of possible board states? 
                    bne      check_next_board_state       ; no, go check this one 
                    ldd      #$DDDD                       ; yes, but this should never happen 
                    std      test_word                    ; trap the error 
                    lbra     test_loop                    ;================================== 

check_next_board_state: 
                    ldx      #Board_State_Pointers        ; get the pointer table for board states 
                    lsla                                  ; offset for the table 
                    ldx      a,x                          ; get address of state to try 
                    ldy      #Play_Board_Status           ; get current play board status 
                    ldb      #10                          ; 9 positions on the board (we start with a decb) 
check_board_state_loop: 
                    decb                                  ; next play board position 
                    beq      found_the_board_state        ; if reached last board position then we found the state 
                    lda      ,x+                          ; get the state board piece 
                    cmpa     ,y+                          ; get the actual board pice 
                    beq      check_board_state_loop       ; if they match then go check next one 
                    inc      board_state_count            ; gone through them all and they didn't match, check next one 
                    lbra     main                         ; go display stuff and come back to check next board state 

found_the_board_state: 
                    lda      board_state_count            ; get the board state counter (represent board state index) 
                    ldx      #Shadow_ML_Model             ; get start address of ML move data in RAM 
                    ldb      #8                           ; offset for pointer table 
                    mul                                   ; calc the table offset 
                    leax     d,x                          ; get start address of move table 
                    clr      board_state_count 
                    lda      #4                           ; 4 possible moves 
                    sta      utility_byte 
vec_from_to_loop 
                    lda      ,x                           ; get 'From' 
                    cmpa     #255                         ; can we do this? 
                    Lbeq     next_vec_from_to             ; no, try next 
                    ldu      Vec_last_move_address        ; save move before this one 
                    stu      Vec_move_before_last_address 
                    stx      Vec_last_move_address        ; save this move 
                    leax     1,x 
                    pshs     a,x                          ; save some registers 
                    ldy      #Play_Board_Blink            ; point to board blin state 
                    ldb      #1                           ; we're blinking 
                    stb      a,y                          ; set the Vec 'From' pawn to blink 
                    lda      #50                          ; delay time 
                    sta      vec_pawn_blink_count 
vec_from_blink: 
                    jsr      draw_board_and_box           ; draw board while delaying 
                    dec      vec_pawn_blink_count         ; decrement the delay counter 
                    bne      vec_from_blink               ; keep delaying 
                    puls     a,x                          ; pull registers from stack 
                    ldy      #Play_Board_Blink            ; stop blinking the Vec pawn 
                    ldb      #0 
                    stb      a,y 
                    ldy      #Play_Board_Status           ; yes, point to top of play board 
                    ldb      #0                           ; place a space in old pawn position 
                    stb      a,y 
                    ldb      #02                          ; place Vec pawn in new position 
                    lda      ,x 
                    stb      a,y 
                    ldy      #Play_Board_Blink            ; point to board blin state 
                    ldb      #1                           ; we're blinking 
                    stb      a,y                          ; set the Vec 'To' pawn to blink 
                    pshs     a                            ; pull registers from stack 
                    lda      #50                          ; delay time 
                    sta      vec_pawn_blink_count 
vec_to_blink: 
                    jsr      draw_board_and_box           ; draw board while delaying 
                    dec      vec_pawn_blink_count         ; decrement the delay counter 
                    bne      vec_to_blink                 ; keep delaying 
                    puls     a                            ; pull registers from stack 
                    ldy      #Play_Board_Blink            ; stop blinking the Vec pawn 
                    ldb      #0 
                    stb      a,y 
                    inc      move_count                   ; add one to the move count 
                    cmpa     #6                           ; Vec wins? 
                    bge      vec_wins 
                    jsr      Check_Player_Can_Move        ; stalemate for Player? 
                    bmi      check_stalemate              ; Vec wins, start again 
                    lda      #1 
                    sta      turn_flag                    ; Player's turn after this 
                    sta      box_selector_mode            ; turn on selector box 
                    lbra     main 

check_stalemate: 
                    lda      players_left_flag 
                    bne      stalemate 
vec_wins: 
                    jsr      Clear_Sound 
                    ldu      #SONG_DATA_Vec               ; init music 
                    jsr      init_ym_sound 
                    jsr      Intensity_5F 
vec_wins_loop: 
                    jsr      Reset0Ref 
                    ldu      #vec_wins_text               ; show intro text 
                    lda      #100 
                    ldb      #-65 
                    jsr      Print_Str_d 
                    jsr      draw_board_and_box 
                    jsr      do_ym_sound                  ; go make some noise 
                    ldd      ym_len                       ; check if end of music 
                    lbeq     intro                        ; yep, start music over 
                    bra      vec_wins_loop                ; no, keep playing music next round 

next_vec_from_to: 
                    leax     1,x 
                    lda      ,x+                          ; point to next from/to pair 
                    dec      utility_byte                 ; flag it 
                    lbne     vec_from_to_loop             ; go check next from/to pair 
                    lda      move_count                   ; check if first move 
                    beq      remove_last_move             ;yes, delete last move 
                    ldx      Vec_move_before_last_address ;yes, delete move before last 
                    lda      #255                         ; delete it 
                    sta      ,x+ 
                    sta      ,x 
                    bra      forfeit 

remove_last_move: 
                    ldx      Vec_last_move_address        ; get last 
delete_move: 
                    lda      #255                         ; delete it 
                    sta      ,x+ 
                    sta      ,x 
                    ldd      #$EEEE 
                    std      test_word                    ; checked them all... stalemate ==================== 
stalemate: 
                    ldu      #SONG_DATA_Stalemate         ; init music 
                    jsr      init_ym_sound 
                    jsr      Intensity_5F 
stalemate_loop: 
                    jsr      Reset0Ref 
                    ldu      #stalemate_text              ; show intro text 
                    lda      #100 
                    ldb      #-50 
                    jsr      Print_Str_d 
                    jsr      draw_board_and_box 
                    jsr      do_ym_sound                  ; go make some noise 
                    ldd      ym_len                       ; check if end of music 
                    lbeq     intro                        ; yep, start music over 
                    bra      stalemate_loop               ; no, keep playing music next round 

                    lbra     intro                        ; and repeat forever 

forfeit: 
                    ldu      #SONG_DARTS_DATA             ; init music 
                    jsr      init_ym_sound 
                    jsr      Intensity_5F 
forfeit_loop: 
                    jsr      Reset0Ref 
                    ldu      #forfeit_text                ; show intro text 
                    lda      #100 
                    ldb      #-50 
                    jsr      Print_Str_d 
                    jsr      draw_board_and_box 
                    jsr      do_ym_sound                  ; go make some noise 
                    ldd      ym_len                       ; check if end of music 
                    lbeq     intro                        ; yep, start music over 
                    bra      forfeit_loop                 ; no, keep playing music next round 

                    lbra     intro                        ; and repeat forever 

test_loop: 
                    bra      test_loop 

;
;***************************************************************************
; SUBROUTINE SECTION
;***************************************************************************
;
;***************************************************************************
;
; This subroutine draws the playboard, the selctor box, and plays selection sound
;
;*****************************************************************************
draw_board_and_box: 
                    ldu      #explosionData               ; point to explosion table entry 
                    jsr      DP_to_C8 
                    jsr      Explosion_Snd                ; set up next sound effect 
                    JSR      Wait_Recal                   ; Vectrex BIOS recalibration 
                    lda      playing_music_flag           ; are we playing a tune? 
                    bne      no_explosion_snd             ; yes, no other sounds 
                    jsr      Do_Sound                     ; this actually plays the sound 
no_explosion_snd 
                    JSR      Intensity_5F                 ; Sets the intensity of the 
                                                          ; vector beam to $5f 
                    lda      #0 
                    sta      utility_byte                 ; start at first board position 
display_board: 
                    ldx      #Play_Board_Status           ; get game board 
                    ldb      utility_byte                 ; get poition on board (0-9) 
                    lda      b, x                         ; load status of this position 
                    cmpa     #00                          ; space? 
                    bne      check_player_pawn            ; no keep checking 
display_board_loop: 
                    inc      utility_byte                 ; next position 
                    lda      utility_byte 
                    cmpa     #9                           ; reached last position on board? 
                    bne      display_board                ; no, keep going 
                    bra      Display_Select_Box           ; yes, back to start 

check_player_pawn: 
                    cmpa     #01                          ; Player is in this position? 
                    bne      check_vectrex_pawn           ; no, check if Vectrex is in this position 
                    ldu      #WhitePawn                   ; display the player pawn piece 
                    bra      display_pawns 

check_vectrex_pawn: 
                    ldu      #BlackPawn                   ; display Vectrex pawn piece 
display_pawns 
                    lda      utility_byte                 ; get pawn position 
                    ldx      #Play_Board_Blink            ; check if we're blinking it 
                    ldb      a,x 
                    beq      draw_pawn                    ; no, draw it as solid pawn 
                    lda      blink_state                  ; yes, flip the draw state of pawn 
                    eora     #1 
                    sta      blink_state 
                    bne      draw_pawn                    ; draw if blink state is 1 
                    bra      display_board_loop           ; go do next pawn 

draw_pawn: 
                    ldx      #upper_left                  ; start of pawn Y/X table 
                    ldb      utility_byte                 ; table position (0-8) 
                    lslb                                  ; multiply by 2 (table offset) 
                    lda      b,x                          ; get the Y position from table 
                    incb     
                    ldb      b,x                          ; get the X position from table 
                    tfr      d,x                          ; place Y/X in X register 
                    lda      #$40                         ; scale positioning 
                    ldb      #$10                         ; scale move in list 
                    jsr      draw_synced_list             ; draw the pawn 
                    bra      display_board_loop           ; go do next pawn 

Display_Select_Box: 
                    lda      box_selector_mode 
                    beq      Draw_Box_Out                 ; skip if we're not showing the box 
                    jsr      Reset0Ref                    ; yes, show it 
                    ldd      #0 
                    jsr      Moveto_d 
                    lda      box_position                 ; get the box position 
                    lsla                                  ; double it 
                    ldy      #box_table                   ; get start address of box table 
                    ldx      a,y                          ; get address of box position vector table 
                    ldb      box_orientation              ; check if drawing top or bottom 
                    beq      Draw_Box                     ; skip if drawing bottom 
                    leax     8,x                          ; we're drawing the top 
Draw_Box: 
                    jsr      Mov_Draw_VLcs                ; draw box 
                    lda      box_orientation              ; flip top<->bottom 
                    eora     1 
                    sta      box_orientation 
Draw_Box_Out: 
                    rts      

;***************************************************************************************
;
; This subroutine check if the player has any moves left
; Returns #255 in REG-A if the player has no moves left
; If 'players_left_flag' > 1 there are still player pawns on the board
;
;****************************************************************************************
Check_Player_Can_Move: 
                    clr      players_left_flag 
                    ldx      #Play_Board_Status           ; get play board status table 
                    lda      #3                           ; check if player is in pos 3 
                    ldb      a,x 
                    cmpb     #1                           ; player pawn there? 
                    bne      check_player_4               ; nope, check next board position 
                    inc      players_left_flag 
                    lda      #0                           ; yes, check directly above pawn 
                    ldb      a,x 
                    bne      check_player_3_capture       ; not open, check if player can capture 
                    rts                                   ; it's open, go back 

check_player_3_capture: 
                    lda      #1                           ; check if player can capture 
                    ldb      a,x 
                    cmpb     #1 
                    ble      check_player_4               ; nope, check next board position 
                    rts                                   ; yes, go back 

check_player_4: 
                    lda      #4                           ; check if player is in pos 4 
                    ldb      a,x 
                    cmpb     #1                           ; player pawn there? 
                    bne      check_player_5               ; nope, check next board position 
                    inc      players_left_flag 
                    lda      #1                           ; yes, check directly above pawn 
                    ldb      a,x 
                    bne      check_player_4_capture_left  ; not open, check if player can capture 
                    rts                                   ; it's open, go back 

check_player_4_capture_left: 
                    lda      #0                           ; check if player can capture 
                    ldb      a,x 
                    cmpb     #1 
                    ble      check_player_4_capture_right ; nope, check next capture 
                    rts                                   ; yes, go back 

check_player_4_capture_right: 
                    lda      #2                           ; check if player can capture 
                    ldb      a,x 
                    cmpb     #1 
                    ble      check_player_5               ; nope, check next board position 
                    rts      

check_player_5: 
                    lda      #5                           ; check if player is in pos 5 
                    ldb      a,x 
                    cmpb     #1                           ; player pawn there? 
                    bne      check_player_6               ; nope, check next board position 
                    inc      players_left_flag 
                    lda      #2                           ; yes, check directly above pawn 
                    ldb      a,x 
                    bne      check_player_5_capture       ; not open, check if player can capture 
                    rts                                   ; it's open, go back 

check_player_5_capture 
                    lda      #1                           ; check if player can capture 
                    ldb      a,x 
                    cmpb     #1 
                    ble      check_player_6               ; nope, check next board position 
                    rts                                   ; yes, go back 

check_player_6: 
                    lda      #6                           ; check if player is in pos 6 
                    ldb      a,x 
                    cmpb     #1                           ; player pawn there? 
                    bne      check_player_7               ; nope, check next board position 
                    inc      players_left_flag 
                    lda      #3                           ; yes, check directly above pawn 
                    ldb      a,x 
                    bne      check_player_6_capture       ; not open, check if player can capture 
                    rts                                   ; it's open, go back 

check_player_6_capture: 
                    lda      #4                           ; check if player can capture 
                    ldb      a,x 
                    cmpb     #1 
                    ble      check_player_7               ; nope, check next board position 
                    rts                                   ; yes, go back 

check_player_7: 
                    lda      #7                           ; check if player is in pos 4 
                    ldb      a,x 
                    cmpb     #1                           ; player pawn there? 
                    bne      check_player_8               ; nope, check next board position 
                    inc      players_left_flag 
                    lda      #4                           ; yes, check directly above pawn 
                    ldb      a,x 
                    bne      check_player_7_capture_left  ; not open, check if player can capture 
                    rts                                   ; it's open, go back 

check_player_7_capture_left: 
                    lda      #3                           ; check if player can capture 
                    ldb      a,x 
                    cmpb     #1 
                    ble      check_player_7_capture_right ; nope, check next capture 
                    rts                                   ; yes, go back 

check_player_7_capture_right: 
                    lda      #5                           ; check if player can capture 
                    ldb      a,x 
                    cmpb     #1 
                    ble      check_player_8               ; nope, check next board position 
                    rts      

check_player_8: 
                    lda      #8                           ; check if player is in pos 6 
                    ldb      a,x 
                    cmpb     #1                           ; player pawn there? 
                    bne      player_stuck                 ; nope, player can't move 
                    inc      players_left_flag 
                    lda      #5                           ; yes, check directly above pawn 
                    ldb      a,x 
                    bne      check_player_8_capture       ; not open, check if player can capture 
                    rts                                   ; it's open, go back 

check_player_8_capture: 
                    lda      #4                           ; check if player can capture 
                    ldb      a,x 
                    cmpb     #1 
                    ble      player_stuck                 ; nope, check next board position 
                    rts                                   ; yes, go back 

player_stuck: 
                    lda      #255 
                    rts      

;************************************************************************************************
;
; This subroutine draws a sync'd list
;
;************************************************************************************************
;ZERO ing the integrators takes time. Measures at my vectrex show e.g.:
;If you move the beam with a to x = -127 and y = -127 at diffferent scale values, the time to reach zero:
;- scale $ff -> zero 110 cycles
;- scale $7f -> zero 75 cycles
;- scale $40 -> zero 57 cycles
;- scale $20 -> zero 53 cycles
ZERO_DELAY          EQU      7                            ; delay 7 counter is exactly 111 cycles delay between zero SETTING and zero unsetting (in moveto_d) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;U = address of vectorlist
;X = (y,x) position of vectorlist (this will be point 0,0), positioning on screen
;A = scalefactor "Move" (after sync)
;B = scalefactor "Vector" (vectors in vectorlist)
;
;     mode, rel y, rel x,                                             
;     mode, rel y, rel x,                                             
;     .      .      .                                                
;     .      .      .                                                
;     mode, rel y, rel x,                                             
;     0x02
; where mode has the following meaning:         
; negative draw line                    
; 0 move to specified endpoint                              
; 1 sync (and move to list start and than to place in vectorlist)      
; 2 end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
draw_synced_list: 
                    pshs     a                            ; remember out different scale factors 
                    pshs     b 
                                                          ; first list entry (first will be a sync + moveto_d, so we just stay here!) 
                    lda      ,u+                          ; this will be a "1" 
sync: 
                    deca                                  ; test if real sync - or end of list (2) 
                    bne      drawdone                     ; if end of list -> jump 
; zero integrators
                    ldb      #$CC                         ; zero the integrators 
                    stb      <VIA_cntl                    ; store zeroing values to cntl 
                    ldb      #ZERO_DELAY                  ; and wait for zeroing to be actually done 
; reset integrators
                    clr      <VIA_port_a                  ; reset integrator offset 
                    lda      #%10000010 
; wait that zeroing surely has the desired effect!
zeroLoop: 
                    sta      <VIA_port_b                  ; while waiting, zero offsets 
                    decb     
                    bne      zeroLoop 
                    inc      <VIA_port_b 
; unzero is done by moveto_d
                    lda      1,s                          ; scalefactor move 
                    sta      <VIA_t1_cnt_lo               ; to timer t1 (lo= 
                    tfr      x,d                          ; load our coordinates of "entry" of vectorlist 
                    jsr      Moveto_d                     ; move there 
                    lda      ,s                           ; scale factor vector 
                    sta      <VIA_t1_cnt_lo               ; to timer T1 (lo) 
moveTo: 
                    ldd      ,u++                         ; do our "internal" moveto d 
                    beq      nextListEntry                ; there was a move 0,0, if so 
                    jsr      Moveto_d 
nextListEntry: 
                    lda      ,u+                          ; load next "mode" byte 
                    beq      moveTo                       ; if 0, than we should move somewhere 
                    bpl      sync                         ; if still positive it is a 1 pr 2 _> goto sync 
; now we should draw a vector 
                    ldd      ,u++                         ;Get next coordinate pair 
                    STA      <VIA_port_a                  ;Send Y to A/D 
                    CLR      <VIA_port_b                  ;Enable mux 
                    LDA      #$ff                         ;Get pattern byte 
                    INC      <VIA_port_b                  ;Disable mux 
                    STB      <VIA_port_a                  ;Send X to A/D 
                    LDB      #$40                         ;B-reg = T1 interrupt bit 
                    CLR      <VIA_t1_cnt_hi               ;Clear T1H 
                    STA      <VIA_shift_reg               ;Store pattern in shift register 
setPatternLoop: 
                    BITB     <VIA_int_flags               ;Wait for T1 to time out 
                    beq      setPatternLoop               ; wait till line is finished 
                    CLR      <VIA_shift_reg               ; switch the light off (for sure) 
                    bra      nextListEntry 

drawdone: 
                    puls     d                            ; correct stack and go back 
                    rts      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;***************************************************************************
; DATA SECTION
;***************************************************************************
intro_text1:        DB       "VEXPAWN", $80
intro_text2:        DB       "SELECT:", $80
intro_text3:        DB       "FRESH START", $80
intro_text4:        DB       "KEEP LEARNING", $80
player_wins_text    DB       "PLAYER WINS!", $80
vec_wins_text:      DB       "VECTREX WINS!", $80
stalemate_text      DB       "STALEMATE",$80
forfeit_text        DB       "FORFEIT", $80
explosionData:      DB       $19,$01,$00,$12 
;vData = VectorList
WhitePawn: 
                    DB       $01, +$2A, -$14              ; sync and move to y, x 
                    DB       $00, +$2A, +$00              ; additional sync move to y, x 
                    DB       $00, +$12, +$00              ; additional sync move to y, x 
                    DB       $FF, -$05, -$09              ; draw, y, x 
                    DB       $FF, -$12, -$09              ; draw, y, x 
                    DB       $FF, -$17, -$01              ; draw, y, x 
                    DB       $FF, -$13, +$0F              ; draw, y, x 
                    DB       $FF, -$0F, -$09              ; draw, y, x 
                    DB       $FF, -$0B, +$0C              ; draw, y, x 
                    DB       $FF, -$1D, -$06              ; draw, y, x 
                    DB       $FF, -$2A, -$1E              ; draw, y, x 
                    DB       $FF, -$12, +$00              ; draw, y, x 
                    DB       $FF, -$0F, +$08              ; draw, y, x 
                    DB       $FF, -$0E, +$1F              ; draw, y, x 
                    DB       $FF, +$00, +$24              ; draw, y, x 
                    DB       $01, -$2A, +$12              ; sync and move to y, x 
                    DB       $00, -$2A, +$00              ; additional sync move to y, x 
                    DB       $00, -$17, +$00              ; additional sync move to y, x 
                    DB       $FF, +$0E, +$1F              ; draw, y, x 
                    DB       $FF, +$13, +$08              ; draw, y, x 
                    DB       $FF, +$16, -$07              ; draw, y, x 
                    DB       $FF, +$1E, -$17              ; draw, y, x 
                    DB       $FF, +$1E, -$07              ; draw, y, x 
                    DB       $FF, +$0E, +$0D              ; draw, y, x 
                    DB       $FF, +$0D, -$08              ; draw, y, x 
                    DB       $FF, +$12, +$0E              ; draw, y, x 
                    DB       $FF, +$1F, +$00              ; draw, y, x 
                    DB       $FF, +$17, -$16              ; draw, y, x 
                    DB       $FF, +$00, -$1C              ; draw, y, x 
                    DB       $FF, -$05, -$09              ; draw, y, x 
                    DB       $02                          ; endmarker 
BlackPawn: 
                    DB       $01, +$33, -$05              ; sync and move to y, x 
                    DB       $00, +$33, +$00              ; additional sync move to y, x 
                    DB       $00, +$06, +$00              ; additional sync move to y, x 
                    DB       $FF, -$06, -$10              ; draw, y, x 
                    DB       $FF, -$0D, -$0D              ; draw, y, x 
                    DB       $FF, -$11, -$05              ; draw, y, x 
                    DB       $FF, -$11, +$02              ; draw, y, x 
                    DB       $FF, -$13, +$0E              ; draw, y, x 
                    DB       $FF, -$11, -$09              ; draw, y, x 
                    DB       $FF, -$10, +$09              ; draw, y, x 
                    DB       $FF, -$12, -$02              ; draw, y, x 
                    DB       $FF, -$12, -$0D              ; draw, y, x 
                    DB       $FF, -$14, -$0F              ; draw, y, x 
                    DB       $FF, -$19, -$04              ; draw, y, x 
                    DB       $FF, -$10, +$0B              ; draw, y, x 
                    DB       $FF, -$0A, +$13              ; draw, y, x 
                    DB       $FF, -$04, +$23              ; draw, y, x 
                    DB       $01, -$33, +$08              ; sync and move to y, x 
                    DB       $00, -$33, +$00              ; additional sync move to y, x 
                    DB       $00, -$06, +$00              ; additional sync move to y, x 
                    DB       $FF, +$06, +$18              ; draw, y, x 
                    DB       $FF, +$0F, +$14              ; draw, y, x 
                    DB       $FF, +$11, +$04              ; draw, y, x 
                    DB       $FF, +$12, -$06              ; draw, y, x 
                    DB       $FF, +$0E, -$0B              ; draw, y, x 
                    DB       $FF, +$0D, -$0B              ; draw, y, x 
                    DB       $FF, +$19, -$05              ; draw, y, x 
                    DB       $FF, +$12, +$08              ; draw, y, x 
                    DB       $FF, +$10, -$06              ; draw, y, x 
                    DB       $FF, +$17, +$0D              ; draw, y, x 
                    DB       $FF, +$15, +$01              ; draw, y, x 
                    DB       $FF, +$10, -$07              ; draw, y, x 
                    DB       $FF, +$0B, -$0F              ; draw, y, x 
                    DB       $FF, +$03, -$16              ; draw, y, x 
                    DB       $01, -$0F, -$19              ; sync and move to y, x 
                    DB       $FF, +$00, +$19              ; draw, y, x 
                    DB       $FF, +$00, +$1A              ; draw, y, x 
                    DB       $02                          ; endmarker 
; Selector Box Positions
line_0_bottom: 
                    DB       2, $7f, 22, -50, 0, 20, 20, 0 
line_0_top: 
                    DB       2, $7f, 38, -50, 20, 0, 0, 20 
line_1_bottom: 
                    DB       2, $7f, 22, -9, 0, 20, 20, 0 
line_1_top: 
                    DB       2, $7f, 38, -9, 20, 0, 0, 20 
line_2_bottom: 
                    DB       2, $7f, 22, 32, 0, 20, 20, 0 
line_2_top: 
                    DB       2, $7f, 38, 32, 20, 0, 0, 20 
line_3_bottom: 
                    DB       2, $7f, -19, -50, 0, 20, 20, 0 
line_3_top: 
                    DB       2, $7f, -3, -50, 20, 0, 0, 20 
line_4_bottom: 
                    DB       2, $7f, -19, -9, 0, 20, 20, 0 
line_4_top: 
                    DB       2, $7f, -3, -9, 20, 0, 0, 20 
line_5_bottom: 
                    DB       2, $7f, -19, 32, 0, 20, 20, 0 
line_5_top: 
                    DB       2, $7f, -3, 32, 20, 0, 0, 20 
line_6_bottom: 
                    DB       2, $7f, -60, -50, 0, 20, 20, 0 
line_6_top: 
                    DB       2, $7f, -45, -50, 20, 0, 0, 20 
line_7_bottom: 
                    DB       2, $7f, -60, -9, 0, 20, 20, 0 
line_7_top: 
                    DB       2, $7f, -45, -9, 20, 0, 0, 20 
line_8_bottom: 
                    DB       2, $7f, -60, 32, 0, 20, 20, 0 
line_8_top: 
                    DB       2, $7f, -45, 32, 20, 0, 0, 20 
box_0               EQU      line_0_bottom 
box_1               EQU      line_1_bottom 
box_2               EQU      line_2_bottom 
box_3               EQU      line_3_bottom 
box_4               EQU      line_4_bottom 
box_5               EQU      line_5_bottom 
box_6               EQU      line_6_bottom 
box_7               EQU      line_7_bottom 
box_8               EQU      line_8_bottom 
box_table: 
                    DW       box_0 
                    DW       box_1 
                    DW       box_2 
                    DW       box_3 
                    DW       box_4 
                    DW       box_5 
                    DW       box_6 
                    DW       box_7 
                    DW       box_8 
;Pawn Screen Position
upper_left: 
                    DB       $50, -$50                    ;upper-left 
upper_center: 
                    DB       $50, -$0                     ;upper-center 
upper_right: 
                    DB       $50, $50                     ;upper_right 
middle_left: 
                    DB       -$0, -$50                    ;middle-left 
middle_center 
                    DB       -$0, -$0                     ;middle-center 
middle_right: 
                    DB       -$0, $50                     ;middle-right 
lower_left: 
                    DB       -$50, -$50                   ;lower-left 
lower_center: 
                    DB       -$50, -$0                    ;lower-center 
lower_right: 
                    DB       -$50, $50                    ;lower-right 
pawn_start_positions: 
                    DB       02, 02, 02 
                    DB       00, 00, 00 
                    DB       01, 01, 01 
player_captures: 
                    DB       0,0                          ; board position 0 (place holder) 
                    DB       0,0                          ; board position 1 (place holder) 
                    DB       0,0                          ; board position 2 (place holder) 
                    DB       1,1                          ; board position 3 
                    DB       0,2                          ; board position 4 
                    DB       1,1                          ; board position 5 
                    DB       4,4                          ; board position 6 
                    DB       3,5                          ; board position 7 
                    DB       4,4                          ; board position 8 
;
S                   EQU      00                           ; Space 
P                   EQU      01                           ; Player 
K                   EQU      02                           ; Vectrex 
Board_States: 
State_1:            DB       K,K,K,P,S,S,S,P,P            ; 1 
State_2:            DB       K,K,K,S,S,P,P,P,S            ; 2 
State_3:            DB       K,K,K,S,P,S,P,S,P            ; 3 
State_4:            DB       K,S,K,K,P,S,S,S,P            ; 4 
State_5:            DB       S,K,K,P,K,S,S,S,P            ; 5 
State_6             DB       K,S,K,P,P,S,S,P,S            ; 6 
State_7:            DB       K,K,S,P,S,P,S,S,P            ; 7 
State_8:            DB       S,K,K,S,K,P,P,S,S            ; 8 
State_9:            DB       S,K,K,K,P,P,P,S,S            ; 9 
State_10:           DB       K,S,K,K,S,P,S,P,S            ; 10 
State_11:           DB       K,K,S,P,P,K,S,S,P            ; 11 
State_12:           DB       S,K,K,P,S,P,P,S,S            ; 12 
State_13:           DB       S,S,K,P,P,P,S,S,S            ; 13 
State_14:           DB       S,K,K,S,P,S,P,S,S            ; 14 
State_15:           DB       K,S,K,P,S,S,S,S,P            ; 15 
State_16:           DB       S,S,K,K,K,P,S,S,S            ; 16 
State_17:           DB       K,S,S,P,P,P,S,S,S            ; 17 
State_18:           DB       S,K,S,K,P,P,S,S,S            ; 18 
State_19:           DB       S,K,S,P,P,K,S,S,S            ; 19 
State_20:           DB       K,S,S,K,K,P,S,S,S            ; 20 
State_21:           DB       K,S,K,P,S,K,S,P,S            ; 21 
State_22:           DB       S,K,K,S,P,S,S,S,P            ; 22 
State_23:           DB       S,S,K,P,K,K,S,S,S            ; 23 
State_24:           DB       S,S,K,K,P,S,S,S,S            ; 24 
State_25:           DB       S,K,S,P,K,S,S,S,S            ; 25 
State_26:           DB       S,K,S,S,K,P,S,S,S            ; 26 
State_27:           DB       K,S,K,S,S,P,P,S,S            ; 27 
State_28:           DB       S,S,K,S,P,K,S,S,S            ; 28 
State_29:           DB       K,S,K,S,P,K,P,S,S            ; 29 
State_30:           DB       K,S,K,S,P,P,S,P,S            ; 30 
State_31:           DB       K,S,K,P,K,P,S,P,S            ; 31 
State_32:           DB       S,K,S,S,P,S,S,S,S            ; 32 
State_33:           DB       K,S,S,K,P,S,S,S,S            ; 33 
State_34:           DB       S,S,K,S,K,P,S,P,S            ; 34 
State_35:           DB       S,K,K,K,S,P,P,P,S            ; 35 
State_36:           DB       S,S,K,P,K,K,S,S,S            ; 36 
State_37:           DB       S,S,K,S,P,K,S,S,S            ; 37 
Board_State_Pointers: 
                    DW       State_1 
                    DW       State_2 
                    DW       State_3 
                    DW       State_4 
                    DW       State_5 
                    DW       State_6 
                    DW       State_7 
                    DW       State_8 
                    DW       State_9 
                    DW       State_10 
                    DW       State_11 
                    DW       State_12 
                    DW       State_13 
                    DW       State_14 
                    DW       State_15 
                    DW       State_16 
                    DW       State_17 
                    DW       State_18 
                    DW       State_19 
                    DW       State_20 
                    DW       State_21 
                    DW       State_22 
                    DW       State_23 
                    DW       State_24 
                    DW       State_25 
                    DW       State_26 
                    DW       State_27 
                    DW       State_28 
                    DW       State_29 
                    DW       State_30 
                    DW       State_31 
                    DW       State_32 
                    DW       State_33 
                    DW       State_34 
                    DW       State_35 
                    DW       State_36 
                    DW       State_37 
ML_Model: 
                    DB       1,4,1,3,2,5,255,255          ; [from,to],[from,to],[from,to] 
                    DB       0,3,1,4,1,5,255,255          ; 2 
                    DB       0,3,2,5,0,4,2,4              ; 3 
                    DB       2,5,0,4,2,4,255,255          ; 4 
                    DB       2,5,1,3,4,7,255,255          ; 5 
                    DB       2,5,0,4,2,4,255,255          ; 6 
                    DB       1,4,1,3,1,5,255,255          ; 7 
                    DB       1,5,4,7,255,255,255,255      ; 8 
                    DB       1,5,2,4,255,255,255,255      ; 9 
                    DB       3,6,3,7,255,255,255,255      ; 10 
                    DB       0,4,1,3,255,255,255,255      ; 11 
                    DB       1,4,1,3,1,5,255,255          ; 12 
                    DB       2,4,255,255,255,255,255,255  ; 13 
                    DB       2,5,2,4,255,255,255,255      ; 14 
                    DB       2,5,255,255,255,255,255,255  ; 15 
                    DB       3,6,4,7,255,255,255,255      ; 16 
                    DB       0,4,255,255,255,255,255,255  ; 17 
                    DB       1,5,3,6,255,255,255,255      ; 18 
                    DB       1,3,5,8,255,255,255,255      ; 19 
                    DB       3,6,4,7,255,255,255,255      ; 20 
                    DB       5,8,5,7,255,255,255,255      ; 21 
                    DB       2,5,2,4,255,255,255,255      ; 22 
                    DB       4,7,5,8,255,255,255,255      ; 23 
                    DB       2,5,2,4,3,6,255,255          ; 24 
                    DB       1,3,4,7,255,255,255,255      ; 25 
                    DB       1,5,1,7,255,255,255,255      ; 26 
                    DB       0,3,255,255,255,255,255,255  ; 27 
                    DB       2,4,5,8,255,255,255,255      ; 28 
                    DB       0,3,0,4,2,4,5,8              ; 29 
                    DB       0,3,0,4,2,4,255,255          ; 30 
                    DB       255,255,255,255,255,255,255,255 ; 31 
                    DB       255,255,255,255,255,255,255,255 ; 32 
                    DB       0,4,3,6,255,255,255,255      ; 33 
                    DB       255,255,255,255,255,255,255,255 ;34 
                    db       1,4,1,5,3,7,255,255          ;35 
                    db       4,7,5,8,255,255,255,255      ;36 
                    db       2,4,5,8,255,255,255,255      ;37 
                    INCLUDE  "Led Storm 6.asm"
                    INCLUDE  "Ultimate_Darts2.asm"
                    INCLUDE  "Supercars7.asm"
                    INCLUDE  "World 2 Finish.asm"
                    INCLUDE  "ymUnpackedPlayer.i"
