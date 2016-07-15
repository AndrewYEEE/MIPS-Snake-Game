#	MIPS SNAKE game
#	Bitmap display options:
#	Unit Width in Pixels:	16
#	Unit Height in Pixels:	 16
#	Display Width in Pixels: 512
#	Display Height in Pixels: 512
#	Base address for display: $gp
#---------------------------------------------------------------------------
#	$s0	snakeaddress address
#	$s1	futureheadaddress
#	$s2	newFirstbody or oldheadaddress
#	$s3	bodylength
#	$s4	food.X
#	$s7	food.Y
#	$s5	(bool) going to eat
#	$s6	growup for 1; common for 0
#	$a3	input ASCII
##########################################################
.data
	outstring: .asciiz "Snake l= "
	#check_error: .asciiz	"OK\n"
	newline: .asciiz "\n"
	side: .half 32			#Give a length 32 of a bitmap
	black: .half 0x0
	growupflag: .half 1
	vertical:	.half 30
	horizontal:	.half 30
	check_error:	.half 	0
	contourcolor: .word 0xabcdef
	foodcolor: .word 0xCC0000
	snakecolor: .word 0xAAAAAAAA
	bodylength: .half 3
	#growupflag: .word 0x01
	startaddress: .word 0x10008000
	snakeaddress: .word 0x0	 	#snakeaddress array
	
	

	

##########################################################################	
.text
init:			
	li $t0, 0x1000819c		#Assign snake address to $t0
	sw $t0, snakeaddress		#Store snake address from $t0 to "snakeaddress"
	jal clear			#Call clear function
	lw $t0, startaddress		#Load display start address
	jal row				#Call the up row function
	jal columns			#Call the column function
	jal row				#Call the bottom row funtion
	jal snake			#Call the snake funtion
	jal food			#Call the food funtion	
	lw $t9,snakecolor
	li $t7,0			#use to check eat self (is a flag)
	li $t6,15			#use to check infinity loop
	li $v1,1			#�i�J�w���Ҧ��e���w�]�ʧ@
	li $k0,0			#�Ω�T�{�e�b��O�_���Y�쭹��
	li $k1,0			#�Ω�T�{��b��O�_���Y�쭹��
################################################################	
headXY:					#get head origin coordinate(if snake alive ,never change)
	la $s0,snakeaddress
	lw $s1,($s0)
	subi $t0,$s1,0x10008000
	div $t0,$t0,0x80
	mfhi $t0
	mflo $s5                         #get headY
	div $s4,$t0,4			 #get headX
	
Strategy:
	li $a0,10		#Decrease to increase difficulty; Increase to decrease difficulty
	li $v0,32			#Sleep for 60 milliseconds
	syscall
		
Direction:
gettailXY:				#get tailXY
	la $s0,snakeaddress		
	lw $s3,bodylength		
	li $t3,30			#check bodylength biger than 30 
	beq $s3,$t3,StrategyS		#if bodylength>30 goto StrategS (Security mode)
	addi $s3,$s3,-1	
	sll $s3,$s3,2			#operate tail coordinate in address: (bodylength-1)*4+first snakeaddress
	add $t2,$s0,$s3			
	lw $t1,($t2)			#get tail address
	subi $t0,$t1,0x10008000		#operate tail coordinate
	div $t0,$t0,0x80
	mfhi $t0
	mflo $t9                         #get tailY
	div $t8,$t0,4			 #get tailX
	#==========================================
	la $s0,snakeaddress		#get this time head coordinate(always change)
	lw $s1,($s0)
	subi $t0,$s1,0x10008000		
	div $t0,$t0,0x80
	mfhi $t0
	mflo $t5                         #get headY
	div $t4,$t0,4			 #get headX

	#================real Strategy====================
	
	slt $t0,$s4,$s6          		 
	bne $t0,$zero,foodXrightofsnakeX    	# hx > foodX
	beq $s4,$s6,foodXrightofsnakeX
	j foodXleftofsnakeX			#hx < foodx


foodXleftofsnakeX:
	slt $t0,$s5,$s7				
	bne $t0,$zero,headYsmallfoodY		#if hy<foody goto headYsmallfoodY(food is under than snake)
	beq $s5,$s7,foodlloop			#if hy==foody goto foodlloop (because food left of snake)
	j headYbigerfoodY			#if hy>foody goto headYbigerfoodY(food is up than snake)
foodlloop:
	beq $s4,$s6,strateEND			#if already eat food goto strateEND(reset head coordinate)
	addi $s4,$s4,-1				#goto left
	j left

foodXrightofsnakeX:	
	slt $t0,$s5,$s7        
	bne $t0,$zero,headYsmallfoodY    #if hy<foody goto headYsmallfoodY(food is under than snake)
	beq $s5,$s7,foodrloop		#if hy==foody goto foodlloop (because food right of snake)
	j headYbigerfoodY		#if hy>foody goto headYbigerfoodY(food is up than snake)
foodrloop:	
	beq $s4,$s6,strateEND		#if already eat food goto strateEND(reset head coordinate)
	addi $s4,$s4,1			#goto right
	j right
headYsmallfoodY:
	addi $s5,$s5,1			#goto under(because food is under than snake)
	j under
	
headYbigerfoodY:
	addi $s5,$s5,-1			#goto up(because food is up than snake)
	j up

strateEND:
	#move $t9,$s5
	#move $t8,$s4
	j headXY			#reset head coordinate
	
	#lw $t0,0xffff0000 	
	#blez $t0, Direction 
	#lw $a3,0xffff0004
	
#Direction:	
	#beq $a3, 0x73,under		#Check if it is 's'
	#beq $a3, 0x64,right		#Check if it is 'd'
	#beq $a3, 0x61,left		#Check if it is 'a'
	#beq $a3, 0x77, up		#Check if it is 'w'
	#j Strategy				#Thread sleep and Key check loop
	
#################################################################	
#execution(NEW snake head address)
under:						
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address to $s2
	addi $s1,$s2,128		#New head address to $s1
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,L1			# �Y�U�@�B�|�Y��ۤv�A�h��V
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,L1			# �Y�U�@�B�|�X�ɡA�h��V
	beq $t7,$0,judge		# t7 �O�@�ӺX�СA�Ω�T�{�O�_�w�g�]���U�@�B�|�Y��ۤv����V����L��V�A�Y���ht7�|��1	
	li $t7,0			#���]�X��t7
	addi $s5,$s5,1			#�]���h���F�o���A�ҥH�n�N���Ƚվ�^�h�A��hx,hy,foodx,foody���Z���t���T
	li $t6,15			#�Ω�P�_�O�_�i�J���`�j��
	j judge				#goto judge	
L1:
	addi $t6,$t6,-1			#��D�]���U�@�B�|�Y��ۤv����V�ɡA���`�j��X�д�@(��D�P�_��V�P�_15������۰ʧP�w�����`)
	beq $t6,$0,final		#��D�P�_��V�P�_15������۰ʧP�w�����`
	bne $t7,$0,ck1			#�Y�H�g�b�P�_���`�j�骺���ΤU($t7=1)�A����ck1
	li $t7,1
	addi $s5,$s5,-1
ck1:
	slt $t2,$t8,$t4			#�Y�D�{�b�Y����m�b���ڥk��A�h�����䨫
	bne $t2,$0,left
	beq $t8,$t4,Lc1			#�Y�D�{�b�Y����m(hx)�M����(tailx)�ۦP�h�i�JLc�P�_��
	j right				#�Y�D�{�b�Y����m�b���ڥ���A�h���k�䨫
Lc1:	
	la $t0,snakeaddress		
	lw $t1,($t0)
	lw $t2,snakecolor
	li  $s0,8
	li  $t3,0
Ld1:	
	addi $s0,$s0,-1			#�����Y���k��ݬO�_���ۤv������
	beq $s0,$0,ext1
	addi $t1,$t1,4
	lw $s1,($t1)
	bne $s1,$t2,Ld1
	addi $t3,$t3,1
ext1:	
	beq $t3,$0,right		#�Y���ۤv������h������
	j left				#�Y�S���ۤv������h���k��
#====================================================================================

right:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	addi $s1,$s2,4			#New head address
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,L2			# �Y�U�@�B�|�Y��ۤv�A�h��V
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,L2			# �Y�U�@�B�|�X�ɡA�h��V
	beq $t7,$0,judge		# t7 �O�@�ӺX�СA�Ω�T�{�O�_�w�g�]���U�@�B�|�Y��ۤv����V����L��V�A�Y���ht7�|��1
	li $t7,0			#���]�X��t7
	addi $s4,$s4,1			#�]���h���F�o���A�ҥH�n�N���Ƚվ�^�h�A��hx,hy,foodx,foody���Z���t���T
	li $t6,15			#�Ω�P�_�O�_�i�J���`�j��
	j judge				#goto judge
	
L2:
	addi $t6,$t6,-1			#��D�]���U�@�B�|�Y��ۤv����V�ɡA���`�j��X�д�@(��D�P�_��V�P�_15������۰ʧP�w�����`)
	beq $t6,$0,final		#��D�P�_��V�P�_15������۰ʧP�w�����`
	bne $t7,$0,ck2			#�Y�H�g�b�P�_���`�j�骺���ΤU($t7=1)�A����ck2
	li $t7,1			
	addi $s4,$s4,-1	
ck2:
	slt $t2,$t9,$t5			#�Y�D�{�b�Y����m�b���ڤU���A�h���W��
	bne $t2,$0,up
	beq $t9,$t5,Lc2			#�Y�D�{�b�Y����m(hy)�M����(taily)�ۦP�h�i�JLc�P�_��
	j under				#�Y�D�{�b�Y����m�b���ڤW���A�h���U��
	
Lc2:	
	la $t0,snakeaddress		#�����Y���W���ݬO�_���ۤv������
	lw $t1,($t0)
	lw $t2,snakecolor
	li  $s0,8
	li  $t3,0
Ld2:	
	addi $s0,$s0,-1	
	beq $s0,$0,ext2
	addi $t1,$t1,-128
	lw $s1,($t1)
	bne $s1,$t2,Ld2
	addi $t3,$t3,1
ext2:	
	beq $t3,$0,up			#�Y���ۤv������h���U��
	j under				#�Y�S���ۤv������h���W�� $t3==0
#==========================================================================

left:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	subi $s1,$s2,4			#New head address
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,L3			# �Y�U�@�B�|�Y��ۤv�A�h��V
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,L3			# �Y�U�@�B�|�X�ɡA�h��V
	beq $t7,$0,judge		# t7 �O�@�ӺX�СA�Ω�T�{�O�_�w�g�]���U�@�B�|�Y��ۤv����V����L��V�A�Y���ht7�|��1
	li $t7,0			#���]�X��t7
	addi $s4,$s4,-1			#�]���h���F�o���A�ҥH�n�N���Ƚվ�^�h�A��hx,hy,foodx,foody���Z���t���T
	li $t6,15			#�Ω�P�_�O�_�i�J���`�j��
	j judge				#goto judge
L3:	
	addi $t6,$t6,-1			#��D�]���U�@�B�|�Y��ۤv����V�ɡA���`�j��X�д�@(��D�P�_��V�P�_15������۰ʧP�w�����`)
	beq $t6,$0,final		#��D�P�_��V�P�_15������۰ʧP�w�����`
	bne $t7,$0,ck3			#�Y�H�g�b�P�_���`�j�骺���ΤU($t7=1)�A����ck3
	li $t7,1
	addi $s4,$s4,1	
ck3:	
	slt $t2,$t9,$t5			#�Y�D�{�b�Y����m�b���ڤU���A�h���W��
	bne $t2,$0,up
	beq $t9,$t5,Lc3			#�Y�D�{�b�Y����m(hy)�M����(taily)�ۦP�h�i�JLc�P�_��
	j under				#�Y�D�{�b�Y����m�b���ڤW���A�h���U��
	
Lc3:	
	la $t0,snakeaddress
	lw $t1,($t0)
	lw $t2,snakecolor
	li  $s0,8
	li  $t3,0
Ld3:	
	addi $s0,$s0,-1			#�����Y���W���ݬO�_���ۤv������
	beq $s0,$0,ext3
	addi $t1,$t1,-128
	lw $s1,($t1)
	bne $s1,$t2,Ld3
	addi $t3,$t3,1
ext3:	
	beq $t3,$0,up			#�Y�S���ۤv������h���W�� $t3==0
	j under				#�Y���ۤv������h���U��
#======================================================================
	
up:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	subi $s1,$s2,128		#New head address
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,L4			# �Y�U�@�B�|�Y��ۤv�A�h��V
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,L4			# �Y�U�@�B�|�X�ɡA�h��V
	beq $t7,$0,judge		# t7 �O�@�ӺX�СA�Ω�T�{�O�_�w�g�]���U�@�B�|�Y��ۤv����V����L��V�A�Y���ht7�|��1
	li $t7,0			#���]�X��t7
	addi $s5,$s5,-1			#�]���h���F�o���A�ҥH�n�N���Ƚվ�^�h�A��hx,hy,foodx,foody���Z���t���T
	li $t6,15			#�Ω�P�_�O�_�i�J���`�j��
	j judge				#goto judge
	
L4:
	addi $t6,$t6,-1			#��D�]���U�@�B�|�Y��ۤv����V�ɡA���`�j��X�д�@(��D�P�_��V�P�_15������۰ʧP�w�����`)
	beq $t6,$0,final		#��D�P�_��V�P�_15������۰ʧP�w�����`
	bne $t7,$0,ck4			#�Y�H�g�b�P�_���`�j�骺���ΤU($t7=1)�A����ck4
	li $t7,1
	addi $s5,$s5,1
ck4:
	slt $t2,$t8,$t4			#�Y�D�{�b�Y����m�b���ڥk��A�h�����䨫
	bne $t2,$0,left		
	beq $t8,$t4,Lc4			#�Y�D�{�b�Y����m(hx)�M����(tailx)�ۦP�h�i�JLc�P�_��
	j right				#�Y�D�{�b�Y����m�b���ڥ���A�h���k�䨫
	
Lc4:	
	la $t0,snakeaddress
	lw $t1,($t0)
	lw $t2,snakecolor
	li  $s0,8
	li  $t3,0
Ld4:	
	addi $s0,$s0,-1			#�����Y���k��ݬO�_���ۤv������
	beq $s0,$0,ext4
	addi $t1,$t1,4
	lw $s1,($t1)
	bne $s1,$t2,Ld4
	addi $t3,$t3,1
ext4:	
	beq $t3,$0,right		#�Y�S���ۤv������h���k��
	j left				#�Y���ۤv������h������
##################################################################
judge:
	
	jal  check_final
	lw	$t0,($s1)		#Load color occupied by the next head address in ($s1)
	lw $t3,foodcolor		#Load foodcolor to $t3
	beq $t0,$t3,growup		#If I eat the food, I grow up
snake_move:	
	lw $t0,snakecolor		#Load snakecolour to $t0
	lw $s3,bodylength		#Load bodylength to $s3
	sw $t0,($s1)			#Colour new head
	sw $s1,($s0)			#Store new head address to "snakeaddress"
	jal snakebody_shift
	j  Strategy



final:
	li $s3,3			#Reset bodylength to 3
	sh $s3,bodylength		
	#lw $a3,0x10010000
	li $a0,500			#Sleep for 0.5s when dead
	li $v0,10
	syscall
	j init			#Return to the game

#function
################################################################################	
#initialization use	function
################################################################################
#Clear the screen (initialize the bitmap)-----------------------------------------------------
clear:
	lw $t0,startaddress		#startaddress = 0x10008000
	lh $t1, side			#Load the side of the bitmap
	mul $t1,$t1,$t1			#Width Pixels * Height Pixels
	lh $t2, black
clearloop:					#Loop until every display address has the black number in it (zero)		
	sw $t2,($t0)			#Initialize all pixels to Black
	addi $t0,$t0,4			#Next pixel
	subi $t1,$t1,1			#pixels need to be initialize = pixels need to be initialize - 1
	bnez $t1,clearloop		#if (pixels need to be initialize != 0) goto clearloop
	jr $ra				#back to mStrategyn

	
#This function is used to draw both the up row and the bottom row-------------------------------
row:								
	lw $t1, contourcolor		#Load contourcolor
	lh $t2, side			#Load the side of the bitmap
drawrow:					#Row loop					
	sw $t1,($t0)			#Draw contour
	addi $t0,$t0,4			#Next pixel
	subi $t2,$t2,1			#pixels need to be draw = pixels need to be draw - 1
	bnez $t2, drawrow		#if (pixels need to be draw != 0) goto drawrow
	jr $ra				#back to mStrategyn
	
	
#Draw the columns--------------------------------------------------------------------------------	
columns:						
	lw $t0, startaddress		#Load the start address
	addi $t0,$t0,128		#Go to the next line: add 0x80
	lh $t2, side			#Load the side of the bitmap
	subi $t2,$t2,2			#Substract the up and down row pixel where x=0 and x=31
drawcolumns:				#Columns Loop
	sw $t1,($t0)			#Draw contourcolor (the countourcolor is still in $t1 after row function was called)
	addi $t0,$t0,124		#Add 31 pixels to the end of the row
	sw $t1,($t0)			#Load contourcolor (draw the column at the end of rows)
	addi $t0,$t0,4			#Next pixel (from the last pixel to the first pixel in next line)
	subi $t2,$t2,1			#pixels need to be draw = pixels need to be draw - 1
	bnez $t2, drawcolumns		#if (pixels need to be draw != 0) goto drawcolumns
	jr $ra				#back to mStrategyn
	
	
#Draw the snake-----------------------------------------------------------------------------------
snake:					
	la $s0,snakeaddress		#Load the address of "snakeaddress"
	lw $s3,bodylength		#Load bodylength
	lw $t0,snakecolor		#Load snakecolor
	lw $t1,($s0)			#Load the address of snake's head which was saved in "snakeaddress" to $t1
drawsnake:				#Snake loop
	sw $t0,($t1)			#Store snake color
	sw $t1,($s0)			#Store next bodypart address to "snakeaddress" array
	subi $t1,$t1,4			#Move $t1 to the next bodypart address (the next bodypart is on the left of the head)
	addi $s0,$s0,4			#Move to the next element of "snakeaddress" array
	subi $s3,$s3,1			#bodylength = bodylength - 1
	bnez $s3, drawsnake		#if (bodylength != 0) goto drawsnake
	jr $ra				#back to mStrategyn
	
	
#Draw the food with  a pseudorandom address----------------------------------------------------------
food:						
	li $a1,0x3c0			#$a1= 32 pixel * (32 - 2)pixel ; food will be randomly gernerate within 0x10008080 ~ (0x10008F80 - 4)
	li $v0,42			#Generate a random number to $a0 ( from 0 to value($a1) )
	syscall		
	sll $a0, $a0, 2			#Multiply $a0 with 4 to generate the address (4 bytes for each pixel)
	add $a0, $a0,0x10008000		#Put the random number on the bitmap address
	lw $t0, ($a0)			#And then check if the new address is already colored
	#lw $t1,snakecolor
	#lw $t2,contourcolor
	bnez $t0,food
	lw $t1,foodcolor		#Load foodcolor
	sw $t1,($a0)			#Finally draw the food
foodXY:  #Calculate food's coordinate
	subi $t0,$a0,0x10008000		#food.X = ( foodaddress - 0x10008000 ) % 0x80 /4
	div $t0,$t0,0x80
	mfhi $t0			
	mflo $s7			#food.Y = ( foodaddress - 0x10008000) / 0x80	
	div $s6,$t0,4			#$s6 = food.X
	#add $t9,$s7,$0
	#add $t8,$s6,$0
	jr $ra
########################################################################################################	
#"judge" use function
########################################################################################################
#----------------------------------------------------------------------------------------------
check_final:			
	lw	$t0,($s1)		#Load color occupied by the next head address in ($s1)
	lw $t1,contourcolor		#Load contourcolor to $t1
	lw $t2,snakecolor		#Load snakecolor to $t2
	beq $t0,$t1, final		#If I hit the countour, I lose
	beq $t0,$t2,final		#If I bit myself, I lose
	jr $ra				#back to common

#snake growup -----------------------------------------------------------------------------------	
growup:		
	
	#li $s6,1			
	#sw $s6,growupflag
	lw $s3,bodylength		
	addi $s3,$s3,1			#Increase bodylength
	sw $s3, bodylength		#Store new bodylength
	lw $t0,snakecolor
	sw $t0,($s1)			#color new head
	lw $s2,($s0)			#Load old head address
	sw $s1,($s0)			#Store new head address to "snakeaddress"	
	#li $s6,1			
	#sh $s6,growupflag	
#print message-----------------------------------------
	li $v0,4
	la $a0, outstring
	syscall
	li $v0, 1
	lw $a0, bodylength
	syscall
	li $v0,4
	la $a0, newline
	syscall
#--------------------------------------------------------
	jal food				#in initialization use function
		
	j	snake_move
	
	
snakebody_shift:
	subi $s3,$s3,1			#bodylength = bodylength - 1
	addi $s0,$s0,4			#Move to the next element of "snakeaddress" array
	lw $t0,($s0)			#Load old bodypart address in "snakeaddress" array to $t0
	sw $s2,($s0)			#Store next new bodypart to "snakeaddress" array
	move $s2,$t0			#Store old bodypart from $s2 to $t0 for being new next bodypart
	bnez $s3,snakebody_shift			#if (bodylength != 0) goto shift	
	lw $t1,($s0)			#Load the tStrategyl from the end of "snakeaddress" array to $t1
	lh $t2,black			#Erase tStrategyl
	sw $t2,($t1)			
	jr $ra				#back to growup or common	
	
	
############################################################################################################


######################################### Security Mode ###############################################
StrategyS:
	li $a0,10			#Decrease to increase difficulty; Increase to decrease difficulty
	li $v0,32			#Sleep for 60 milliseconds
	syscall
	
	li $t0,1			
	beq $s5,$t0,Loop1		#�Y�{�b�D�b�̤W�h�h�i�J�w��mode
	beq $v1,$zero,Loop1		#�N��i�Jsecurity�Ҧ�($v1==1�ɡA�D�|���Q��k������W��ɡA�Ӥ�����security mode)
	addi $s5,$s5,-1
	j upS
DirectionS:
#=========================================================================
Loop1:					#(�K�ۤW��ɨ�)
	li $v1,0			#v1==0�N��D�����i�Jsecurity mode
	bne $a2,$0,Loop2		#�Y�W����٨S�����A$a2==0
	slti $t0,$s4,30			#X�b�̻��u���30�A�P�_�O�_�w���쩳
	beq $t0,$0,Loop2		#�Y�w���쩳�A�h����loop2
	bne $k0,$0,LL1			#k0==1�N��{�b�O�i�H�Y���������A�A$k0==0�h�N����w�Y�쭹���A
					#�����D�^�����(�H�̵u���|)�A�����D�~��Y����
	addi $s4,$s4,1
	j rightS

LL1:	
	beq $s4,$s6,Loop2		#�Y�{�b�D����m(hx)�M����(foodx)�ۦP�A�h����loop2(���U��)
	addi $s4,$s4,1			
	j rightS
#=========================================================================	
Loop2:					#(�K�ۥk��ɨ�or���F�䭹���ө��U��)
	li $v1,0			#v1==0�N��D�����i�Jsecurity mode
	li $a2,1			#�N��W��ɨ����A$a2==1
	bne $a3,$0,Loop3		#�Y�k����٨S�����A$a3==0
	slti $t0,$s5,30			#y�b�̻��u���30�A�P�_�O�_�w���쩳
	beq $t0,$0,Loop3		#�Y�w���쩳�A�h����loop3
	#beq $s5,$s7,ck
	bne $k0,$0,LL2			#k0==1�N��{�b�O�i�H�Y���������A�A$k0==0�h�N����w�Y�쭹��
	addi $s5,$s5,1
	j underS
LL2:
	beq $s5,$s7,ckk1		#�Y�{�b�D����m(hy)�M����(foody)�ۦP�A�h����ckk1�N��w�g�Y�쭹��
	addi $s5,$s5,1			#�Y�S���h�~�򩹤U��
	j underS			
ckk1:
	li $a2,0			#�]$a2==0�O�]�����]��$k0==1�ɡA�bLL1�P�_hx==foodx�ɴN������loop2(�V�U��)�A
					#�Ӥ��O�]��$s4�w�g����30�~���U���A�ҥH�b���n�]$a2==0���D��x�b����(����30)
	li $k0,0			#�]�w�g�Y�쭹���A�]$k0==0�h�N����w�Y�쭹��
	j Loop1
#=========================================================================
Loop3:					#(�K�ۤU��ɨ�)
	li $v1,0			#v1==0�N��D�����i�Jsecurity mode
	li $a3,1			#�N��k��ɨ����A$a3==1
	slti $t0,$s4,2			#�P�_hx�O�_�H����U��ɪ��̥���(x==1)
	bne $t0,$zero,Loop4		#�Y���h����loop4
	bne $k1,$0,LL3			#k1==1�N��{�b�O�i�H�Y���������A�A$k1==0�h�N����w�Y�쭹��
	subi $s4,$s4,1
	j leftS
LL3:
	beq $s4,$s6,Loop4		#�Y�{�b�D����m(hx)�M����(foodx)�ۦP�A�h����loop4(���W��)
	subi $s4,$s4,1
	j leftS
#=========================================================================
Loop4:					#(�K�ۥ���ɨ�)
	slti $t0,$s5,2			#�P�_hy�O�_�H���쥪��ɪ��̤W��(y==1)
	bne $t0,$0,check		#�Y���h����check�����]���ʧ@
	bne $k1,$0,LL4			#k1==1�N��{�b�O�i�H�Y���������A�A$k1==0�h�N����w�Y�쭹��
	subi $s5,$s5,1
	j upS
	
LL4:
	beq $s5,$s7,ckk2		#�Y�{�b�D����m(hy)�M����(foody)�ۦP�A�h����ckk2�N��w�g�Y�쭹��
	subi $s5,$s5,1
	j upS
ckk2:	
	li $k1,0			#�]�w�g�Y�쭹���A�]$k1==0�h�N����w�Y�쭹��
	j Loop3				#�b���n���D��x�b����(����1)
#=========================================================================
check:					#(¶���@��X�Э��])
	li $a2,0			#�Ω�T�{�W��ɬO�_����
	li $a3,0			#�Ω�T�{�k��ɬO�_����
	li $k0,1			#�Ω�T�{�e�b��O�_���Y�쭹��
	li $k1,1			#�Ω�T�{��b��O�_���Y�쭹��
	j DirectionS
	
#################################################################	
#execution(NEW snake head address)
underS:						
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address to $s2
	addi $s1,$s2,128		#New head address to $s1
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,Ls1			# �Y�U�@�B�|�Y��ۤv�A�h��V
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls1			# �Y�U�@�B�|�X�ɡA�h��V
	beq $t7,$0,judgeS		# t7 �O�@�ӺX�СA�Ω�T�{�O�_�w�g�]���U�@�B�|�Y��ۤv����V����L��V�A�Y�S�ht7�|��1	
	li $t7,0			#���]�X��t7
	addi $s5,$s5,1			#�]���h���F�o���A�ҥH�n�N���Ƚվ�^�h�A��hx,hy,foodx,foody���Z���t���T
	li $t6,15			#�Ω�P�_�O�_�i�J���`�j��
	j judgeS			#goto judge	
Ls1:
	addi $t6,$t6,-1
	beq $t6,$0,finalS
	bne $t7,$0,rightS
	li $t7,1
	addi $s5,$s5,-1
	j rightS
rightS:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	addi $s1,$s2,4			#New head address
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,Ls2			# �Y�U�@�B�|�Y��ۤv�A�h��V
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls2			# �Y�U�@�B�|�X�ɡA�h��V
	beq $t7,$0,judgeS		# t7 �O�@�ӺX�СA�Ω�T�{�O�_�w�g�]���U�@�B�|�Y��ۤv����V����L��V�A�Y�S�ht7�|��1
	li $t7,0			#���]�X��t7
	addi $s4,$s4,1			#�]���h���F�o���A�ҥH�n�N���Ƚվ�^�h�A��hx,hy,foodx,foody���Z���t���T
	li $t6,15			#�Ω�P�_�O�_�i�J���`�j��
	j judgeS			#goto judge
Ls2:
	addi $t6,$t6,-1
	beq $t6,$0,finalS
	bne $t7,$0,upS
	li $t7,1
	addi $s4,$s4,-1
	j upS
leftS:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	subi $s1,$s2,4			#New head address
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,Ls3			# �Y�U�@�B�|�Y��ۤv�A�h��V
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls3			# �Y�U�@�B�|�X�ɡA�h��V
	beq $t7,$0,judgeS		# t7 �O�@�ӺX�СA�Ω�T�{�O�_�w�g�]���U�@�B�|�Y��ۤv����V����L��V�A�Y�S�ht7�|��1
	li $t7,0			#���]�X��t7
	addi $s4,$s4,-1			#�]���h���F�o���A�ҥH�n�N���Ƚվ�^�h�A��hx,hy,foodx,foody���Z���t���T
	li $t6,15			#�Ω�P�_�O�_�i�J���`�j��
	j judgeS			#goto judge
Ls3:
	addi $t6,$t6,-1
	beq $t6,$0,finalS
	bne $t7,$0,underS
	li $t7,1
	addi $s4,$s4,1
	j underS
upS:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	subi $s1,$s2,128		#New head address
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,Ls4			# �Y�U�@�B�|�Y��ۤv�A�h��V
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls4			# �Y�U�@�B�|�X�ɡA�h��V
	beq $t7,$0,judgeS		# t7 �O�@�ӺX�СA�Ω�T�{�O�_�w�g�]���U�@�B�|�Y��ۤv����V����L��V�A�Y�S�ht7�|��1
	li $t7,0			#���]�X��t7
	addi $s5,$s5,-1			#�]���h���F�o���A�ҥH�n�N���Ƚվ�^�h�A��hx,hy,foodx,foody���Z���t���T
	li $t6,15			#�Ω�P�_�O�_�i�J���`�j��
	j judgeS			#goto judge
	
Ls4:
	addi $t6,$t6,-1
	beq $t6,$0,finalS
	bne $t7,$0,leftS
	li $t7,1
	addi $s5,$s5,1
	j leftS
##################################################################
judgeS:
	
	jal  check_finalS
	lw	$t0,($s1)		#Load color occupied by the next head address in ($s1)
	lw $t3,foodcolor		#Load foodcolor to $t3
	beq $t0,$t3,growupS		#If I eat the food, I grow up
snake_moveS:	
	lw $t0,snakecolor		#Load snakecolour to $t0
	lw $s3,bodylength		#Load bodylength to $s3
	sw $t0,($s1)			#Colour new head
	sw $s1,($s0)			#Store new head address to "snakeaddress"
	jal snakebody_shiftS
	j  StrategyS



finalS:
	li $s3,3			#Reset bodylength to 3
	sh $s3,bodylength		
	#lw $a3,0x10010000
	li $a0,500			#Sleep for 0.5s when dead
	li $v0,10
	syscall
	j init			#Return to the game

#function
################################################################################	
#initialization use	function
################################################################################
#Clear the screen (initialize the bitmap)-----------------------------------------------------

	
#This function is used to draw both the up row and the bottom row-------------------------------

#Draw the columns--------------------------------------------------------------------------------	
	
#Draw the snake-----------------------------------------------------------------------------------
	
	
#Draw the food with  a pseudorandom address----------------------------------------------------------

########################################################################################################	
#"judge" use function
########################################################################################################
#----------------------------------------------------------------------------------------------
check_finalS:			
	lw	$t0,($s1)		#Load color occupied by the next head address in ($s1)
	lw $t1,contourcolor		#Load contourcolor to $t1
	lw $t2,snakecolor		#Load snakecolor to $t2
	beq $t0,$t1, finalS	#If I hit the countour, I lose
	beq $t0,$t2,finalS		#If I bit myself, I lose
	jr $ra				#back to common

#snake growup -----------------------------------------------------------------------------------	
growupS:		
	
	#li $s6,1			
	#sw $s6,growupflag
	lw $s3,bodylength		
	addi $s3,$s3,1			#Increase bodylength
	sw $s3, bodylength		#Store new bodylength
	lw $t0,snakecolor
	sw $t0,($s1)			#color new head
	lw $s2,($s0)			#Load old head address
	sw $s1,($s0)			#Store new head address to "snakeaddress"	
#print message-----------------------------------------
	li $v0,4
	la $a0, outstring
	syscall
	li $v0, 1
	lw $a0, bodylength
	syscall
	li $v0,4
	la $a0, newline
	syscall
#--------------------------------------------------------
	jal food				#in initialization use function
		
	j	snake_moveS
	
	
snakebody_shiftS:
	subi $s3,$s3,1			#bodylength = bodylength - 1
	addi $s0,$s0,4			#Move to the next element of "snakeaddress" array
	lw $t0,($s0)			#Load old bodypart address in "snakeaddress" array to $t0
	sw $s2,($s0)			#Store next new bodypart to "snakeaddress" array
	move $s2,$t0			#Store old bodypart from $s2 to $t0 for being new next bodypart
	bnez $s3,snakebody_shiftS			#if (bodylength != 0) goto shift	
	lw $t1,($s0)			#Load the tStrategyl from the end of "snakeaddress" array to $t1
	lh $t2,black			#Erase tStrategyl
	sw $t2,($t1)			
	jr $ra				#back to growup or common	

	
	
############################################################################################################


############################################################################################################
