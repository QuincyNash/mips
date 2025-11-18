.data
array:      .word   88, 39, -5, 100, -1092, 10, -10, 0  # 0 = sentinel
comma:      .asciiz ", "
newline:    .asciiz "\n"

.text
            .globl  main
main:
    la      $t0,            array                       # pointer to array
    move    $t1,            $zero                       # length counter

find_len:
    lw      $t2,            0($t0)
    beq     $t2,            $zero,      len_done
    addi    $t1,            $t1,        1
    addi    $t0,            $t0,        4
    b       find_len

len_done:
    move    $a0,            $zero                       # start index = 0
    addi    $a1,            $t1,        -1              # end index = length-1
    jal     quicksort

    # ----------------------
    # Print array with commas
    # ----------------------
    la      $t0,            array

print_loop:
    lw      $t1,            0($t0)
    beq     $t1,            $zero,      done

    move    $a0,            $t1
    li      $v0,            1
    syscall

    lw      $t2,            4($t0)
    beq     $t2,            $zero,      skip_comma

    la      $a0,            comma
    li      $v0,            4
    syscall

skip_comma:
    addi    $t0,            $t0,        4
    b       print_loop

done:
    la      $a0,            newline
    li      $v0,            4
    syscall
    li      $v0,            10
    syscall

    # ===================================================
    # quicksort(start, end)
    # a0 = start index, a1 = end index
    # Uses s0 to hold original end across recursive calls.
    # Uses only core instructions (slt/beq/bne etc).
    # ===================================================
quicksort:
    addi    $sp,            $sp,        -12
    sw      $ra,            8($sp)
    sw      $s0,            4($sp)
    sw      $s1,            0($sp)                      # save used callee-saved regs (s1 unused but preserved)

    # base case: if start >= end return
    slt     $t0,            $a0,        $a1
    beq     $t0,            $zero,      qs_return

    move    $s0,            $a1                         # save original end in s0

    # pivot = array[end]
    la      $t1,            array
    sll     $t2,            $a1,        2
    addu    $t2,            $t2,        $t1
    lw      $t3,            0($t2)                      # pivot in t3

    move    $t4,            $a0                         # i = start
    move    $t5,            $a0                         # j = start

partition_loop:
    # loop while j < end  (do NOT include pivot itself)
    slt     $t0,            $t5,        $a1
    beq     $t0,            $zero,      partition_done

    # load A[j]
    la      $t1,            array
    sll     $t6,            $t5,        2
    addu    $t6,            $t6,        $t1
    lw      $t7,            0($t6)                      # t7 = A[j]

    # if A[j] <= pivot  --> swap A[i],A[j] ; i++
    slt     $t0,            $t3,        $t7             # t0 = (pivot < A[j])
    bne     $t0,            $zero,      next_j          # if pivot < A[j], skip swap
    # swap A[i] and A[j]
    sll     $t8,            $t4,        2
    addu    $t8,            $t8,        $t1
    lw      $t9,            0($t8)                      # t9 = A[i]
    sw      $t7,            0($t8)                      # A[i] = A[j]
    sw      $t9,            0($t6)                      # A[j] = old A[i]
    addi    $t4,            $t4,        1               # i++

next_j:
    addi    $t5,            $t5,        1
    b       partition_loop

partition_done:
    # swap pivot (A[end]) with A[i]
    sll     $t6,            $t4,        2
    addu    $t6,            $t6,        $t1             # addr of A[i]
    sll     $t7,            $s0,        2
    addu    $t7,            $t7,        $t1             # addr of A[original end]
    lw      $t8,            0($t6)
    lw      $t9,            0($t7)
    sw      $t9,            0($t6)
    sw      $t8,            0($t7)

    # recursive left: quicksort(start, i-1) if start < i-1
    addi    $t0,            $t4,        -1
    slt     $t1,            $a0,        $t0
    beq     $t1,            $zero,      skip_left
    move    $a0,            $a0
    move    $a1,            $t0
    jal     quicksort

skip_left:
    # recursive right: quicksort(i+1, original_end) if i+1 < original_end
    addi    $t0,            $t4,        1
    slt     $t1,            $t0,        $s0
    beq     $t1,            $zero,      skip_right
    move    $a0,            $t0
    move    $a1,            $s0
    jal     quicksort

skip_right:
    # epilogue / restore and return
    lw      $ra,            8($sp)
    lw      $s0,            4($sp)
    lw      $s1,            0($sp)
    addi    $sp,            $sp,        12
    jr      $ra

qs_return:
    lw      $ra,            8($sp)
    lw      $s0,            4($sp)
    lw      $s1,            0($sp)
    addi    $sp,            $sp,        12
    jr      $ra
