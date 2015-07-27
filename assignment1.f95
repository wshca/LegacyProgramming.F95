! Assignment 1 : Legacy Fortran
!   Hangman
!
! Student Name:   Wolfgang Huettinger
! Student ID:     
! Student Email:  
!
! Date created:  January 14th 2013
! Date modified: February 4th 2013
!
! Modified Fortran program from original source written in F77 which is a conversion by
!    M. Wirth in April 2012. This program is based on a Basic program written by
!    Ken Aupperle, Half Hallow Hulls H.S. Dix Hills, NY

program hangman
   implicit none

   character dictionaryItem(20), alphabet(26)
   integer usedWord(50)
   integer wordIndexMax, wordIndex
   character*20 dictionary(50)
   wordIndex=0
   wordIndexMax=50

   ! Start the game
   write (*,*) "THE GAME OF HANGMAN"

   ! Call subroutine to setup the game
   call gamesetup(dictionaryItem,alphabet,usedWord,dictionary)

   ! Outer while loop to make sure we play the game until the end
   do while (wordIndex < wordIndexMax)
      call playthegame(dictionaryItem,alphabet,usedWord,wordIndexMax,wordIndex,dictionary)
   end do

   ! Finish the game
   write (*,*) "Ending..."

end program hangman

! Subroutine to setup the game
subroutine gamesetup(dictionaryItem,alphabet,usedWord,dictionary)
   implicit none
   integer I,J
   character, intent(inout) :: dictionaryItem(20), alphabet(26)
   integer, intent(inout) :: usedWord(50)
   character*20, intent(inout) :: dictionary(50)

   dictionary = [character(20) :: 'gum','sin','for','cry','lug','bye','fly','ugly', 'each','from','work', &
    'talk','with','self','pizza','thing','feign','fiend','elbow','fault', &
    'dirty','budget','spirit','quaint','maiden','escort','pickax','example', &
    'tension','quinine',  'kidney','replica','sleeper','triangle','kangaroo', &
    'mahogany','sergeant','sequence', 'moustache','dangerous','scientist', &
    'different','quiescent','magistrate','erroneously', 'loudspeaker', &
    'phytotoxic','matrimonial','parasympathomimetic','thigmotropism']

   do I = 1, 20
      dictionaryItem(I) = "-"
   end do

   do I = 1, 26
      alphabet(I) = " "
   end do

   do I = 1, 50
      usedWord(I) = 0
   end do

end subroutine

! Play the game's iteration
subroutine playthegame(dictionaryItem,alphabet,usedWord,wordIndexMax,wordIndex,dictionary)
   implicit none
   integer I, J, randomIndex, numberOfGuesses, lengthSelectedWord, rigthLetterCounter, wrongLetterCounter, runForEver
   character asciiPicture(12,12), selectedWord*20, guessedChar, guessedWord*20, PlayNextGame
   character, intent(inout) :: dictionaryItem(20), alphabet(26)
   integer, intent(inout) ::  usedWord(50), wordIndexMax, wordIndex
   character*20, intent(inout) :: dictionary(50)
   integer acceptableGuess, storeIndex, guessIncomplete, runSubGame, runonce

   !Declare the function so we can use it later in this subroutine
   integer :: randomnumber

   ! Present the picture each iteration
   do I = 1, 12
      do J = 1, 12
         asciiPicture(I,J) = " "
      end do
   end do

   do I = 1, 12
      asciiPicture(I,1) = "X"
   end do

   do J = 1, 7
      asciiPicture(1,J) = "X"
   end do

   asciiPicture(2,7) = "X"

   ! Randomly generate index which then used for the dictionery selection.
   ! Make sure the word hasn't been used. If it has been used generate another index.
   runForever=0
   do while (runForEver==0)
      randomIndex = randomnumber(wordIndexMax) !Random generator
      if (usedWord(randomIndex) == 0) then
         runForEver=1 !As the word hasn't been used yet, we can exit the loop.
      end if
   end do

   ! Setup the variables used for this word
   usedWord(randomIndex) = 1
   wrongLetterCounter=0
   wordIndex = wordIndex+1
   numberOfGuesses = 0
   selectedWord = dictionary(randomIndex)
   lengthSelectedWord = LEN_TRIM(selectedWord)
   write(*,*) dictionaryItem(1:lengthSelectedWord)
   write(*,*) "Here are the letters you used: "
   do I = 1, 26
      if (alphabet(I+1)==" ") then
         exit
      end if
      write(*,'(AA$)') alphabet(I),","
   end do

   ! Run for ever until the maximum of wrong guesses is reached or the word is guess right
   runSubGame=1
   do while(runSubGame==1)

      ! Ask the user for a character until an acceptable is entered. Acceptable characters aren't element of N.
      acceptableGuess=0
      do while (acceptableGuess==0)
         write(*,*) " "
         write(*,*) "What is your guess? "; rigthLetterCounter=0
         read(*,*) guessedChar

         ! Runonce is used to ensure we only show the notification of already guessed word once
         runonce=0
         do I = 1, 26
            ! We have an character we already used before
            if ((alphabet(I) == guessedChar) .and. (runonce==0)) then
               write(*,*) "You guessed that letter before"
               runonce=1
            end if
            ! We found an acceptable character for the guess value
            if (alphabet(I) == " ") then
               acceptableGuess=1
               storeIndex = I
            end if
         end do
      end do

      ! Check if the character is already in the vector with the existing guesses
      alphabet(storeIndex)=guessedChar
      numberOfGuesses=numberOfGuesses+1 ! Guess counter which is incremented if wrong guessed
      do I = 1, lengthSelectedWord
         if (selectedWord(I:I) == guessedChar) then
            !If character is new, add it to the vector of guesses character and increment counter for guesses
            dictionaryItem(I) = guessedChar
            rigthLetterCounter = rigthLetterCounter+1
         end if
      end do

      ! Write out the hidden word to the user every time even after the user used up this character
      if (rigthLetterCounter == 0) then
         wrongLetterCounter=wrongLetterCounter+1
         write (*,*) "Sorry, that letter isn't in the word."
         call drawHangman(asciiPicture,wrongLetterCounter)
      end if

      guessIncomplete=0
      do I = 1,lengthSelectedWord
         if (dictionaryItem(I)== "-") then
            guessIncomplete=1
         end if
      end do

      if (guessIncomplete==1) then
         ! Display word and ask user to guess the wordIndexMax
         write (*,*) dictionaryItem(1:lengthSelectedWord)
         write (*,*) "What is your guess for the word? "
         read (*,*) guessedWord
         if (selectedWord == guessedWord) then
            write (*,*) "Right! It took you ",numberOfGuesses," guesses"
         end if
      else
         ! Display success message and end game
         write (*,*) "You found the word."
         runSubGame=0
      end if

      if (wrongLetterCounter==9) then
         runSubGame=0
         write (*,*) "Sorry, you loose. The word was ", selectedWord
         write (*,*) "You missed that one."
      end if

write(*,*) "guess counter",wrongLetterCounter

   end do ! End of the while loop which is running for this subgame

   write (*,*) "Do you want another word? (Y/N) "
   read (*,*) PlayNextGame

   ! Ask user if he want to play another game
   if (PlayNextGame=="Y") then
      ! Set the word counter to the maximum so we finish the game
      wordIndex = wordIndexMax
   end if

end subroutine

! Draw the handman when the user is wrong
subroutine drawHangman(asciiPicture,wrongLetterCounter)
   character, intent(inout) :: asciiPicture(12,12)
   integer, intent(in) :: wrongLetterCounter
   integer :: I,J

   ! Note with this select statement it is no longer necessary to write the asciiPicture variable a second
   !  time if we are at the second position and so on. Unlike in the GOTO statement where the syntax was writing
   !  the same data over again.
   select case (wrongLetterCounter)
   case (1)
      write (*,*) "First we draw a head."
      asciiPicture(3,6) = "-"; asciiPicture(3,7) = "-"; asciiPicture(3,8) = "-"; asciiPicture(4,5) = "("; asciiPicture(4,6) = "."
   case (2)
      write (*,*) "Now we draw a body."
      asciiPicture(4,8) = "."; asciiPicture(4,9) = ")"; asciiPicture(5,6) = "-"; asciiPicture(5,7) = "-"; asciiPicture(5,8) = "-"
   case (3)
      write (*,*) "Next we draw an arm."
      do I = 4,7
         asciiPicture(I,I-1) = "\"
      end do
   case (4)
      write (*,*) "This time it's the other arm."
      asciiPicture(4,11) = "/"; asciiPicture(5,10) = "/"; asciiPicture(6,9) = "/"; asciiPicture(7,8) = "/";
   case (5)
      write (*,*) "Now, let's draw the right leg."
      asciiPicture(10,6) = "/"; asciiPicture(11,5) = "/"
   case (6)
      write (*,*) "This time we draw the left leg."
      asciiPicture(10,8) = "\"; asciiPicture(11,9) = "\"
   case (7)
      write (*,*) "Now we put up a hand."
      asciiPicture(3,11) = "\"
   case (8)
      write (*,*) "Next the other hand."
      asciiPicture(3,3) = "/"
   case (9)
      write (*,*) "Now we draw one foot."
      asciiPicture(12,10) = "\"; asciiPicture(12,11) = "-"
   case default
      write (*,*) "Here's the other foot -- You're hung!!."
      asciiPicture(12,3) = "-"; asciiPicture(12,4) = "/"
   end select

   ! Draw the previous set ascii picture on the screen
   do I = 1,12
      write (*,*) (asciiPicture(I,J),J=1,12)
   end do

end subroutine drawHangman

! Return an integer in the range
integer function randomnumber(wordIndexMax)
   implicit none
   integer, intent(in) :: wordIndexMax
   integer :: rndFloor, randominteger, seedValue, clock, n, i
   real :: harvest
   integer, dimension(:), allocatable :: newSeed

   ! Functions is available in Fortran 95 and documentation is under gcc.gnu.org
   ! The following code is very similar to the example given in GCC compiler documentation
   ! Determine the size of the seed to be used later and dynamically allocated
   call random_seed(size = n)
   allocate(newSeed(n))

   ! Get the system as a base for the random seed to generate more sudo random numbers
   call system_clock(count=clock)

   ! Adapt the time variable
   newSeed = clock + 37 * (/ (i-1,i=1,n)/)

   ! Call the function to setup the seed at every iteration
   call random_seed(put=newSeed)

   ! Call the function to gather the next random number
   call random_number(harvest)

   ! Adapt the number accordingly to the 1-wordIndexMax(which is often 50) to select a word in the range
   randomnumber = floor(wordIndexMax*harvest)+1 ! Set the function to the value of the next highest interger

   return
end function randomnumber

