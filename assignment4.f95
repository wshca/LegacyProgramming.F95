! Assignment 4 : Legacy Fortran
!   Tic-Tac-Toe
!
! Student Name:   Wolfgang Huettinger
! Student ID:     
! Student Email:  
!
! Date created:  April 3rd 2013
! Date modified: April 9th 2013
!
! This program is a modern adaption of a Fortran77 program. The program can be found in
! the appendix A of Multics Fortran User's Guide by Honeywell, December 1979, order number CC70-01
! It can be found under the link:
! http://www.bitsavers.org/pdf/honeywell/multics/CC70-01C_fortranUG_Dec83.pdf
!
program tictactoe
   ! This main function is supposed to be only the main caller
   ! This enables future development to be conducted at this modular way
   implicit none

   ! Declare some variables for later use
   integer board(9)
   logical polite
   data polite /.true./
   integer i

   ! Pre-defining the array to make ensure correctness
   do i = 1,9
      board(i) = 1
   end do

   ! Call the actual game routine
   call rungame(polite,board)

end program tictactoe


subroutine rungame(polite,board)
  implicit none

  logical, intent(inout) :: polite
  integer, intent(inout) :: board(9)

  logical playgame /.true./
  logical wonthegame
  logical wonthegame1
  integer userchoice
  integer empty, his, mine
  integer i, j
  parameter (empty= 1)
  parameter (his  = 2)
  parameter (mine = 3)
  character*1 symbol(3) /" ", "x", "o"/

  do while (playgame)
     ! Ask user for a valid choice
     call getuserchoice(polite,board,userchoice)
     ! See if this move won game for the player
     call won(board,userchoice,wonthegame)
     if (wonthegame .eqv. .true.) then
        write(*,*) " You win!"
        playgame = .false.
     end if
     if (playgame .eqv. .true.) then
        ! The player hasn't won yet, and it is the machine's turn,
        ! so it is necessary to get a machine move from the
        ! subroutine, mover.
        call mover(board,userchoice)
     end if
     if (playgame .eqv. .true.) then
        ! Check move to see if game is drawn.
        if (userchoice == 0) then
           write(*,*) " Cat's game."
           playgame = .false.
        end if
     end if
     if (playgame .eqv. .true.) then
        ! Game is not draw, so inform player of machine's
        ! move and put move on board.
        write(*,52) userchoice
        52 format(" My move is ", i1)
        board(userchoice) = mine
        ! Check whether machine has just won.
        ! The won function does it.
        call won(board,userchoice,wonthegame)
        if (wonthegame .eqv. .true.) then
           write(*,*) " I win!"
           playgame = .false.
        else
           ! Print the board as it is at the current state
           call printtheboard(board)
        end if
     end if
   end do
   ! Come here at game's end, regardless of outcome.
   ! Print the board as it is at the current state
   call printtheboard(board)
end subroutine rungame

subroutine getuserchoice(polite,board,userchoice)
  implicit none

  logical, intent(inout) :: polite
  integer, intent(inout) :: board(9)
  integer, intent(inout) :: userchoice

  logical notvalidchoice
  integer empty, his, mine
  parameter (empty= 1)
  parameter (his  = 2)
  parameter (mine = 3)

  notvalidchoice = .true.
  do while (notvalidchoice)
     if (polite .eqv. .true.) then
        polite = .false.
        call unpolitemessage
     end if
     ! Request user input.
     ! NOTE: Invalid input like characters or anything else then an integer will crash the program!
     write(*,*) " Your move?"
     read (*,66) userchoice
     66 format(i2)
     if (userchoice>0 .and. userchoice <= 9) then
        if (board(userchoice) == his) then
           write(*,100) "You",userchoice
           100 format(1x, a3, " have already played ", i1, ".")
        else if (board(userchoice) == mine) then
           write(*,100) "  I",userchoice
        else
           notvalidchoice = .false.
           board(userchoice) = his
        end if
     else
        ! Input was not accepted therefore the prompt will appear again
        write (*,*) "Invalid input."
     end if
  end do
end subroutine getuserchoice

subroutine unpolitemessage
   ! This is the message displayed only once while the game starts up
   write(*,*) "Play tic-tac-toe. Type 1-9 to play."
   write(*,*) ""
   write(*,*) "          1|2|3"
   write(*,*) "          4|5|6"
   write(*,*) "          7|8|9"
   write(*,*) ""
end subroutine unpolitemessage

subroutine printtheboard(board)
   implicit none

   integer, intent(in) :: board(9)
   integer i,j
   character element(3)

   ! Go through the vector and write it out to a 3x3 matrix seperated by "|"
   do i = 0,6,3
       do j = 1,3
       ! Get the substitute for the number representing a symbol x,o or blank
       if (board(j+i) == 1) then
          element(j) = " "
       end if
       if (board(j+i) == 2) then
          element(j) = "x"
       end if
       if (board(j+i) == 3) then
          element(j) = "o"
       end if
       end do
       ! Print out the line
       write(*,*) " ", element(1), "|", element(2), "|", element(3)
   end do

end subroutine printtheboard

subroutine mover(board,userchoice)
  implicit none

  integer, intent(inout) :: board(9)

  integer, intent(inout) :: userchoice
  integer i,j,k,l,m
  integer empty, his, mine
  parameter (empty= 1)
  parameter (his  = 2)
  parameter (mine = 3)

  ! All possible paths of the game. Each path has three cells. Each number represents a cell.
  ! The numbers are ordered to correspond to the path along which a game can be won.
  integer paths(3,8)
     data paths /1,2,3, 4,5,6, 7,8,9, 1,4,7, 2,5,8, 3,6,9, 1,5,9, 3,5,8/

  ! The number of the paths that pass through a given cell. No cell has more than
  ! 4 paths through it (center)
  ! The corner cells each have 3 paths through them. The rest have 2. 0 represents no path.
  integer pathsThroughCell (4,9)
     data pathsThroughCell /1,4,7,0, 1,5,0,0, 1,6,8,0, 2,4,0,0, 2,5,7,0, 2,6,0,0, 3,4,8,0, 3,5,0,0, 3,6,7,0/

  ! Holds the path sum, or the sum of the weights of the different states of a cell
  integer pathsum(8)

  ! Weight for the three states of the cells in order "empty", "his", and "mine"
  integer weight(3) /0,1,4/

  ! Order in which we will choose to use cell w
  integer cells(9)
     data cells /5,1,3,7,9,2,4,6,8/

  ! New approach to make the computer win - Comment out if the user wants higher chances to win
  ! Block the center if available
  if (board(5) == 1) then
     userchoice = 5
     return
  end if
  ! Block the non-edge fields if empty
  do i = 2,8,2
     if (board(i) == 1) then
        userchoice = i
        return
     end if
  end do

  ! Calculuate the pathsum, note that k is the counter
  do i = 1, 8
     pathsum(i) = 0
     do j = 1, 3
        k = board(paths(i,j))
        pathsum(i) = pathsum(i) + weight(k)
     end do
  end do

  ! Find path with two in a row for me, and play a third cell to win
  ! Improved to avoid clutter
  do j = 1, 8
     if (pathsum(j) == (weight(his)*2) .or. pathsum(j) == (weight(mine)*2)) then
        do i = 1,3
           userchoice = paths (j,j)
           if (board(userchoice) == empty) then
              return
           end if
        end do
     end if
  end do

  ! Try to make two two-on-a-rows for me (offensive move, rule)
  !   For each cell, start counting at 0 for each pathsThroughCell,
  !   If there is no path, escape for each cell;
  !   count the number of paths through the cell that have
  !   a pathsum of 4. If it finds two or more
  !   such paths through one cell,
  !   the machine
  !   moves inte that call.
  ! Furthermore add:
  ! Try to block two-in-a-row for player (defensive move, rule 4)
  do userchoice = 1, 9
     if(board(userchoice) == empty) then
        k = 0
        do l = 1, 4
           if (pathsThroughCell(l,userchoice) /= 0) then
              if (pathsum(pathsThroughCell(l,userchoice)) == weight(mine)) then
                 return
              end if
              if (pathsum(pathsThroughCell(l,userchoice)) == weight(his)) then
                 return
              end if
           end if
        end do
     end if
  end do

  ! No offensive or defensive move so just pick a cell (rule 5)
  do i = 1,9
     userchoice = cells(i) ! Look through cells in order of priority
     if (board(userchoice) == empty) then
        return
     end if
  end do

  ! No move is found so the game is a draw
  userchoice = 0 !0 means "draw" to caller
end subroutine mover

subroutine won(board,userchoice,wonthegame)
  implicit none

  integer, intent(inout) :: board(9)
  logical, intent(inout) :: wonthegame

  integer, intent(inout) :: userchoice
  integer i,j,k,l,m
  integer empty, his, mine
  parameter (empty= 1)
  parameter (his  = 2)
  parameter (mine = 3)

  integer board2d(3,3), xORo, x, y
  logical horizontal, vertical, diagonal1, diagonal2
  horizontal = .true.
  vertical  = .true.
  xORo = board(userchoice)

  ! Do a manual: equivalence(board2d,board)
  do i = 1, 3
     do j = 1, 3
        board2d(i,j) = board(i+((j-1)*3))
     end do
  end do

  ! Note that
  !     x is row
  !     y is collumn
  x = mod(userchoice,3)     ! Extract the row
  y = (userchoice-1) / 3 + 1 ! Extract the collumn

  ! Check horizonal and vertical simulatanously
  do i = 1, 3
     ! Found a cell on this path that is in different state
     if (board2d(x,i) /= xORo) then
        horizontal = .false.
     end if
     if (board2d(i,y) /= xORo) then
        vertical = .false.
     end if
  end do

  ! Is the cell on a diagonal?
  ! Here I use a new approach to detect diagonal wins simpler and easier to read
  ! Case 1: Is Cell on left-to-right downward diagonal?
  if (board(1) == board (5) .and. board(5) == board(9)) then
     diagonal1 = .true.
  else
     diagonal1 = .false.
  end if

  ! Case 2: Is Cell on left-to-right upward diagonal?
  if (board(3) == board (5) .and. board(5) == board(7)) then
     diagonal2 = .true.
  else
     diagonal2 = .false.
  end if

  ! Case 3: Not on diagonal in the else case as well if both prior estimates of diagonal wins are evaluated
  if ( (diagonal1 .eqv. .false.) .and. (diagonal2 .eqv. .false.)) then
     wonthegame = horizontal .or. vertical
  else
     do i = 1,3
        if (board2d(i,i) /= xORo) then
           diagonal1 = .false.
        end if
        if (board2d(i,4-i) /= xORo) then
           diagonal2 = .false.
        end if
     end do
     wonthegame = horizontal .or. vertical .or. diagonal1 .or. diagonal2
  end if
end subroutine won
