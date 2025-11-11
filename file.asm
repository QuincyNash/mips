.data
msg:        .asciiz "Number: "
newline:    .asciiz "\n"

.text
main:
    li      $t0,    1                           # counter = 1

loop:
    li      $t1,    10                          # limit = 100
    ble     $t0,    $t1,        print_number
    j       end

print_number:
    li      $v0,    4                           # syscall: print string
    la      $a0,    msg
    syscall

    li      $v0,    1                           # syscall: print integer
    move    $a0,    $t0
    syscall

    li      $v0,    4                           # print newline
    la      $a0,    newline
    syscall

    addi    $t0,    $t0,        1
    j       loop

end:
    li      $v0,    10                          # exit
    syscall
