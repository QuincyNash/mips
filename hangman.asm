.data
filename:           .asciiz "dict.txt"
                    .align  2
word:               .space  128                                             # buffer for selected word (127 + null)
onech:              .space  1                                               # temp 1-byte buffer for file reads

    # hangman data
                    .align  2
guessed:            .space  128
input:              .space  8
guessed_letters:    .space  26
                    .align  2
letters_count:      .word   0
max_wrong:          .word   6

msg_no_file:        .asciiz "Could not open dictionary\n"
msg_no_words:       .asciiz "Dictionary contains no words\n"
msg_prompt:         .asciiz "Guess a letter: "
msg_win:            .asciiz "You won!\n"
msg_lose:           .asciiz "You lost! The word was: "
msg_wrong:          .asciiz "Wrong guesses: "
msg_repeat:         .asciiz "Letter already guessed! Try again.\n"
msg_invalid:        .asciiz "Please enter a letter (A-Z).\n"
newline:            .asciiz "\n"

.text
                    .globl  main
main:
    nop
    ############################################################################
    # --- Read a random word from dict.txt into 'word' (non-empty, <=31 chars)-
    ############################################################################

read_try_loop:
    # open file
    li      $v0,                13
    la      $a0,                filename
    li      $a1,                0                                           # read-only
    li      $a2,                0
    syscall
    bltz    $v0,                file_open_failed
    move    $t0,                $v0                                         # t0 = fd

    # count lines robustly
    li      $t1,                0                                           # line_count
    li      $t9,                0                                           # last_char_non_nl flag
count_loop:
    li      $v0,                14                                          # read(fd, buf, 1)
    move    $a0,                $t0
    la      $a1,                onech
    li      $a2,                1
    syscall
    beq     $v0,                $zero,              count_done              # EOF
    lb      $t2,                onech
    li      $t3,                10                                          # '\n'
    beq     $t2,                $t3,                inc_line
    move    $t9,                $t2
    j       count_loop
inc_line:
    addi    $t1,                $t1,                1
    li      $t9,                0
    j       count_loop
count_done:
    bnez    $t9,                inc_last_line
    j       lines_count_done
inc_last_line:
    addi    $t1,                $t1,                1
lines_count_done:
    move    $t4,                $t1                                         # t4 = total_lines

    # if no lines, close & error
    beq     $t4,                $zero,              no_words_found

    # pick a random line index in [0, total_lines-1]
    li      $a0,                0
    move    $a1,                $t4                                         # syscall 42 returns [a0, a1)
    li      $v0,                42
    syscall
    move    $t5,                $v0                                         # t5 = chosen_line (0-based)

    # seek to start
    li      $v0,                19
    move    $a0,                $t0
    li      $a1,                0
    li      $a2,                0
    syscall

    # if chosen_line == 0 -> start reading first line
    beq     $t5,                $zero,              start_line_read_setup

    # otherwise skip chosen_line newline boundaries
    li      $t6,                0                                           # newlines_seen
skip_lines_loop:
    li      $v0,                14
    move    $a0,                $t0
    la      $a1,                onech
    li      $a2,                1
    syscall
    beq     $v0,                $zero,              eof_while_skipping
    lb      $t7,                onech
    li      $t8,                10
    beq     $t7,                $t8,                inc_newline_seen
    j       skip_lines_loop
inc_newline_seen:
    addi    $t6,                $t6,                1
    beq     $t6,                $t5,                start_line_read_setup
    j       skip_lines_loop

eof_while_skipping:
    # Unexpected EOF while skipping; retry
    li      $v0,                16
    move    $a0,                $t0
    syscall
    j       read_try_loop

start_line_read_setup:
    # read characters into word buffer until newline or EOF or maxlen
    la      $t9,                word
    li      $t3,                0                                           # idx
read_word_loop:
    li      $v0,                14
    move    $a0,                $t0
    la      $a1,                onech
    li      $a2,                1
    syscall
    beq     $v0,                $zero,              finish_read_word
    lb      $t2,                onech
    li      $t7,                10
    beq     $t2,                $t7,                finish_read_word
    # if we have space (limit 31)
    li      $t8,                31
    bge     $t3,                $t8,                ignore_extra_chars
    sb      $t2,                0($t9)
    addi    $t9,                $t9,                1
    addi    $t3,                $t3,                1
    j       read_word_loop
ignore_extra_chars:
    j       read_word_loop

finish_read_word:
    # null-terminate
    sb      $zero,              0($t9)
    # if empty line, retry
    beq     $t3,                $zero,              empty_line_retry

    # close file and proceed
    li      $v0,                16
    move    $a0,                $t0
    syscall
    j       hangman_start

empty_line_retry:
    li      $v0,                16
    move    $a0,                $t0
    syscall
    j       read_try_loop

file_open_failed:
    la      $a0,                msg_no_file
    li      $v0,                4
    syscall
    li      $v0,                10
    syscall

no_words_found:
    # close fd then complain and exit
    li      $v0,                16
    move    $a0,                $t0
    syscall
    la      $a0,                msg_no_words
    li      $v0,                4
    syscall
    li      $v0,                10
    syscall

    ############################################################################
    # --- Hangman game start: 'word' buffer is filled, null-terminated
    ############################################################################

hangman_start:
    # initialize hangman registers
    la      $s0,                word                                        # base of word
    la      $s1,                guessed
    la      $s4,                input
    la      $s6,                guessed_letters
    lw      $s3,                max_wrong
    li      $s2,                0                                           # wrong = 0
    li      $s7,                0                                           # letters_count = 0

    # compute word length -> $s5
    li      $t0,                0
    la      $t1,                word
len_loop2:
    lb      $t2,                0($t1)
    beq     $t2,                $zero,              len_done2
    addi    $t0,                $t0,                1
    addi    $t1,                $t1,                1
    j       len_loop2
len_done2:
    move    $s5,                $t0

    # initialize guessed[] with underscores
    li      $t0,                0
init_guess2:
    beq     $t0,                $s5,                init_done2
    li      $t2,                95                                          # underscore
    sb      $t2,                0($s1)
    addi    $s1,                $s1,                1
    addi    $t0,                $t0,                1
    j       init_guess2
init_done2:
    sb      $zero,              0($s1)
    la      $s1,                guessed

main_loop:
    # print guessed state with spaces
    li      $t0,                0
print_loop:
    beq     $t0,                $s5,                print_loop_done
    addu    $t3,                $s1,                $t0
    lb      $t4,                0($t3)
    move    $a0,                $t4
    li      $v0,                11
    syscall
    li      $a0,                32
    li      $v0,                11
    syscall
    addi    $t0,                $t0,                1
    j       print_loop
print_loop_done:
    la      $a0,                newline
    li      $v0,                4
    syscall

    # print wrong count
    la      $a0,                msg_wrong
    li      $v0,                4
    syscall
    move    $a0,                $s2
    li      $v0,                1
    syscall
    la      $a0,                newline
    li      $v0,                4
    syscall

    # prompt input
    la      $a0,                msg_prompt
    li      $v0,                4
    syscall
    la      $a0,                input
    li      $a1,                8
    li      $v0,                8
    syscall

    # get first char
    lb      $t0,                0($s4)
    beq     $t0,                $zero,              main_loop

    # ----- validate: only letters allowed (A-Z or a-z). lowercase -> uppercase -----
    li      $t1,                65                                          # 'A'
    li      $t2,                90                                          # 'Z'
    blt     $t0,                $t1,                check_lower_case
    bgt     $t0,                $t2,                check_lower_case
    # it's uppercase letter -> proceed
    j       input_is_letter

check_lower_case:
    li      $t1,                97                                          # 'a'
    li      $t2,                122                                         # 'z'
    blt     $t0,                $t1,                invalid_input
    bgt     $t0,                $t2,                invalid_input
    addi    $t0,                $t0,                -32                     # convert to uppercase
    j       input_is_letter

invalid_input:
    la      $a0,                msg_invalid
    li      $v0,                4
    syscall
    j       main_loop

input_is_letter:
    # check repeated letter against guessed_letters ($s6), count $s7
    li      $t1,                0
check_repeat:
    beq     $t1,                $s7,                not_repeated
    addu    $t2,                $s6,                $t1
    lb      $t3,                0($t2)
    beq     $t3,                $t0,                repeat_found
    addi    $t1,                $t1,                1
    j       check_repeat
repeat_found:
    la      $a0,                msg_repeat
    li      $v0,                4
    syscall
    j       main_loop
not_repeated:
    # store new guessed letter
    addu    $t2,                $s6,                $s7
    sb      $t0,                0($t2)
    addi    $s7,                $s7,                1

    # search the word for the guessed letter
    li      $t3,                0
    li      $t4,                0                                           # found flag
search_loop2:
    beq     $t3,                $s5,                search_done2
    addu    $t5,                $s0,                $t3
    lb      $t6,                0($t5)
    beq     $t6,                $t0,                matched_here2
    addi    $t3,                $t3,                1
    j       search_loop2
matched_here2:
    addu    $t7,                $s1,                $t3
    sb      $t0,                0($t7)
    li      $t4,                1
    addi    $t3,                $t3,                1
    j       search_loop2
search_done2:
    beq     $t4,                $zero,              not_found2

    # check win (no underscores left)
    li      $t0,                0
    li      $t5,                0
check_loop2:
    beq     $t0,                $s5,                check_done2
    addu    $t6,                $s1,                $t0
    lb      $t7,                0($t6)
    li      $t1,                95
    beq     $t7,                $t1,                underscore_found2
    addi    $t0,                $t0,                1
    j       check_loop2
underscore_found2:
    li      $t5,                1
check_done2:
    beq     $t5,                $zero,              win
    j       main_loop

not_found2:
    addi    $s2,                $s2,                1
    bge     $s2,                $s3,                lose
    j       main_loop

win:
    # print full word spaced
    li      $t0,                0
print_full_word:
    beq     $t0,                $s5,                full_word_done
    addu    $t1,                $s0,                $t0
    lb      $t2,                0($t1)
    move    $a0,                $t2
    li      $v0,                11
    syscall
    li      $a0,                32
    li      $v0,                11
    syscall
    addi    $t0,                $t0,                1
    j       print_full_word
full_word_done:
    la      $a0,                newline
    li      $v0,                4
    syscall
    la      $a0,                msg_win
    li      $v0,                4
    syscall
    j       done

lose:
    # print lose message
    la      $a0,                msg_lose
    li      $v0,                4
    syscall

    # print the raw word (no spaces)
    la      $a0,                word
    li      $v0,                4
    syscall

    # final newline
    la      $a0,                newline
    li      $v0,                4
    syscall

    j       done

done:
    li      $v0,                10
    syscall
