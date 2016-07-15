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
	li $v1,1			#進入安全模式前的預設動作
	li $k0,0			#用於確認前半圈是否有吃到食物
	li $k1,0			#用於確認後半圈是否有吃到食物
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
	beq $t1,$t0,L1			# 若下一步會吃到自己，則轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,L1			# 若下一步會出界，則轉向
	beq $t7,$0,judge		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若有則t7會為1	
	li $t7,0			#重設旗標t7
	addi $s5,$s5,1			#因為多走了這部，所以要將插值調整回去，讓hx,hy,foodx,foody的距離差正確
	li $t6,15			#用於判斷是否進入死亡迴圈
	j judge				#goto judge	
L1:
	addi $t6,$t6,-1			#當蛇因為下一步會吃到自己而轉向時，死亡迴圈旗標減一(當蛇判斷轉向判斷15次之後自動判定為死亡)
	beq $t6,$0,final		#當蛇判斷轉向判斷15次之後自動判定為死亡
	bne $t7,$0,ck1			#若以經在判斷死亡迴圈的情形下($t7=1)，跳至ck1
	li $t7,1
	addi $s5,$s5,-1
ck1:
	slt $t2,$t8,$t4			#若蛇現在頭的位置在尾巴右邊，則往左邊走
	bne $t2,$0,left
	beq $t8,$t4,Lc1			#若蛇現在頭的位置(hx)和尾巴(tailx)相同則進入Lc判斷式
	j right				#若蛇現在頭的位置在尾巴左邊，則往右邊走
Lc1:	
	la $t0,snakeaddress		
	lw $t1,($t0)
	lw $t2,snakecolor
	li  $s0,8
	li  $t3,0
Ld1:	
	addi $s0,$s0,-1			#先往頭的右邊看是否有自己的身體
	beq $s0,$0,ext1
	addi $t1,$t1,4
	lw $s1,($t1)
	bne $s1,$t2,Ld1
	addi $t3,$t3,1
ext1:	
	beq $t3,$0,right		#若有自己的身體則往左走
	j left				#若沒有自己的身體則往右走
#====================================================================================

right:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	addi $s1,$s2,4			#New head address
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,L2			# 若下一步會吃到自己，則轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,L2			# 若下一步會出界，則轉向
	beq $t7,$0,judge		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若有則t7會為1
	li $t7,0			#重設旗標t7
	addi $s4,$s4,1			#因為多走了這部，所以要將插值調整回去，讓hx,hy,foodx,foody的距離差正確
	li $t6,15			#用於判斷是否進入死亡迴圈
	j judge				#goto judge
	
L2:
	addi $t6,$t6,-1			#當蛇因為下一步會吃到自己而轉向時，死亡迴圈旗標減一(當蛇判斷轉向判斷15次之後自動判定為死亡)
	beq $t6,$0,final		#當蛇判斷轉向判斷15次之後自動判定為死亡
	bne $t7,$0,ck2			#若以經在判斷死亡迴圈的情形下($t7=1)，跳至ck2
	li $t7,1			
	addi $s4,$s4,-1	
ck2:
	slt $t2,$t9,$t5			#若蛇現在頭的位置在尾巴下面，則往上走
	bne $t2,$0,up
	beq $t9,$t5,Lc2			#若蛇現在頭的位置(hy)和尾巴(taily)相同則進入Lc判斷式
	j under				#若蛇現在頭的位置在尾巴上面，則往下走
	
Lc2:	
	la $t0,snakeaddress		#先往頭的上面看是否有自己的身體
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
	beq $t3,$0,up			#若有自己的身體則往下走
	j under				#若沒有自己的身體則往上走 $t3==0
#==========================================================================

left:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	subi $s1,$s2,4			#New head address
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,L3			# 若下一步會吃到自己，則轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,L3			# 若下一步會出界，則轉向
	beq $t7,$0,judge		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若有則t7會為1
	li $t7,0			#重設旗標t7
	addi $s4,$s4,-1			#因為多走了這部，所以要將插值調整回去，讓hx,hy,foodx,foody的距離差正確
	li $t6,15			#用於判斷是否進入死亡迴圈
	j judge				#goto judge
L3:	
	addi $t6,$t6,-1			#當蛇因為下一步會吃到自己而轉向時，死亡迴圈旗標減一(當蛇判斷轉向判斷15次之後自動判定為死亡)
	beq $t6,$0,final		#當蛇判斷轉向判斷15次之後自動判定為死亡
	bne $t7,$0,ck3			#若以經在判斷死亡迴圈的情形下($t7=1)，跳至ck3
	li $t7,1
	addi $s4,$s4,1	
ck3:	
	slt $t2,$t9,$t5			#若蛇現在頭的位置在尾巴下面，則往上走
	bne $t2,$0,up
	beq $t9,$t5,Lc3			#若蛇現在頭的位置(hy)和尾巴(taily)相同則進入Lc判斷式
	j under				#若蛇現在頭的位置在尾巴上面，則往下走
	
Lc3:	
	la $t0,snakeaddress
	lw $t1,($t0)
	lw $t2,snakecolor
	li  $s0,8
	li  $t3,0
Ld3:	
	addi $s0,$s0,-1			#先往頭的上面看是否有自己的身體
	beq $s0,$0,ext3
	addi $t1,$t1,-128
	lw $s1,($t1)
	bne $s1,$t2,Ld3
	addi $t3,$t3,1
ext3:	
	beq $t3,$0,up			#若沒有自己的身體則往上走 $t3==0
	j under				#若有自己的身體則往下走
#======================================================================
	
up:
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address
	subi $s1,$s2,128		#New head address
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,L4			# 若下一步會吃到自己，則轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,L4			# 若下一步會出界，則轉向
	beq $t7,$0,judge		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若有則t7會為1
	li $t7,0			#重設旗標t7
	addi $s5,$s5,-1			#因為多走了這部，所以要將插值調整回去，讓hx,hy,foodx,foody的距離差正確
	li $t6,15			#用於判斷是否進入死亡迴圈
	j judge				#goto judge
	
L4:
	addi $t6,$t6,-1			#當蛇因為下一步會吃到自己而轉向時，死亡迴圈旗標減一(當蛇判斷轉向判斷15次之後自動判定為死亡)
	beq $t6,$0,final		#當蛇判斷轉向判斷15次之後自動判定為死亡
	bne $t7,$0,ck4			#若以經在判斷死亡迴圈的情形下($t7=1)，跳至ck4
	li $t7,1
	addi $s5,$s5,1
ck4:
	slt $t2,$t8,$t4			#若蛇現在頭的位置在尾巴右邊，則往左邊走
	bne $t2,$0,left		
	beq $t8,$t4,Lc4			#若蛇現在頭的位置(hx)和尾巴(tailx)相同則進入Lc判斷式
	j right				#若蛇現在頭的位置在尾巴左邊，則往右邊走
	
Lc4:	
	la $t0,snakeaddress
	lw $t1,($t0)
	lw $t2,snakecolor
	li  $s0,8
	li  $t3,0
Ld4:	
	addi $s0,$s0,-1			#先往頭的右邊看是否有自己的身體
	beq $s0,$0,ext4
	addi $t1,$t1,4
	lw $s1,($t1)
	bne $s1,$t2,Ld4
	addi $t3,$t3,1
ext4:	
	beq $t3,$0,right		#若沒有自己的身體則往右走
	j left				#若有自己的身體則往左走
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
	beq $s5,$t0,Loop1		#若現在蛇在最上層則進入安全mode
	beq $v1,$zero,Loop1		#代表進入security模式($v1==1時，蛇會先想辦法先走到上邊界，而不執行security mode)
	addi $s5,$s5,-1
	j upS
DirectionS:
#=========================================================================
Loop1:					#(貼著上邊界走)
	li $v1,0			#v1==0代表蛇正式進入security mode
	bne $a2,$0,Loop2		#若上邊界還沒走完，$a2==0
	slti $t0,$s4,30			#X軸最遠只能到30，判斷是否已走到底
	beq $t0,$0,Loop2		#若已走到底，則跳到loop2
	bne $k0,$0,LL1			#k0==1代表現在是可以吃食物的狀態，$k0==0則代表剛剛已吃到食物，
					#並讓蛇回到邊框(以最短路徑)，不讓蛇繼續吃食物
	addi $s4,$s4,1
	j rightS

LL1:	
	beq $s4,$s6,Loop2		#若現在蛇的位置(hx)和食物(foodx)相同，則跳至loop2(往下走)
	addi $s4,$s4,1			
	j rightS
#=========================================================================	
Loop2:					#(貼著右邊界走or為了找食物而往下走)
	li $v1,0			#v1==0代表蛇正式進入security mode
	li $a2,1			#代表上邊界走完，$a2==1
	bne $a3,$0,Loop3		#若右邊界還沒走完，$a3==0
	slti $t0,$s5,30			#y軸最遠只能到30，判斷是否已走到底
	beq $t0,$0,Loop3		#若已走到底，則跳到loop3
	#beq $s5,$s7,ck
	bne $k0,$0,LL2			#k0==1代表現在是可以吃食物的狀態，$k0==0則代表剛剛已吃到食物
	addi $s5,$s5,1
	j underS
LL2:
	beq $s5,$s7,ckk1		#若現在蛇的位置(hy)和食物(foody)相同，則跳至ckk1代表已經吃到食物
	addi $s5,$s5,1			#若沒有則繼續往下走
	j underS			
ckk1:
	li $a2,0			#設$a2==0是因為剛剛因為$k0==1時，在LL1判斷hx==foodx時就先跳到loop2(向下走)，
					#而不是因為$s4已經走到30才往下走，所以在此要設$a2==0讓蛇把x軸走完(走到30)
	li $k0,0			#因已經吃到食物，設$k0==0則代表剛剛已吃到食物
	j Loop1
#=========================================================================
Loop3:					#(貼著下邊界走)
	li $v1,0			#v1==0代表蛇正式進入security mode
	li $a3,1			#代表右邊界走完，$a3==1
	slti $t0,$s4,2			#判斷hx是否以走到下邊界的最左邊(x==1)
	bne $t0,$zero,Loop4		#若有則跳至loop4
	bne $k1,$0,LL3			#k1==1代表現在是可以吃食物的狀態，$k1==0則代表剛剛已吃到食物
	subi $s4,$s4,1
	j leftS
LL3:
	beq $s4,$s6,Loop4		#若現在蛇的位置(hx)和食物(foodx)相同，則跳至loop4(往上走)
	subi $s4,$s4,1
	j leftS
#=========================================================================
Loop4:					#(貼著左邊界走)
	slti $t0,$s5,2			#判斷hy是否以走到左邊界的最上面(y==1)
	bne $t0,$0,check		#若有則跳至check做重設的動作
	bne $k1,$0,LL4			#k1==1代表現在是可以吃食物的狀態，$k1==0則代表剛剛已吃到食物
	subi $s5,$s5,1
	j upS
	
LL4:
	beq $s5,$s7,ckk2		#若現在蛇的位置(hy)和食物(foody)相同，則跳至ckk2代表已經吃到食物
	subi $s5,$s5,1
	j upS
ckk2:	
	li $k1,0			#因已經吃到食物，設$k1==0則代表剛剛已吃到食物
	j Loop3				#在此要讓蛇把x軸走完(走到1)
#=========================================================================
check:					#(繞完一圈旗標重設)
	li $a2,0			#用於確認上邊界是否走完
	li $a3,0			#用於確認右邊界是否走完
	li $k0,1			#用於確認前半圈是否有吃到食物
	li $k1,1			#用於確認後半圈是否有吃到食物
	j DirectionS
	
#################################################################	
#execution(NEW snake head address)
underS:						
	la $s0, snakeaddress		#Load the head address to $s0
	lw $s2,($s0)			#Load old head address to $s2
	addi $s1,$s2,128		#New head address to $s1
	
	lw $t0,snakecolor		#load snake color
	lw $t1,($s1)			#load next step's color
	beq $t1,$t0,Ls1			# 若下一步會吃到自己，則轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls1			# 若下一步會出界，則轉向
	beq $t7,$0,judgeS		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若又則t7會為1	
	li $t7,0			#重設旗標t7
	addi $s5,$s5,1			#因為多走了這部，所以要將插值調整回去，讓hx,hy,foodx,foody的距離差正確
	li $t6,15			#用於判斷是否進入死亡迴圈
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
	beq $t1,$t0,Ls2			# 若下一步會吃到自己，則轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls2			# 若下一步會出界，則轉向
	beq $t7,$0,judgeS		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若又則t7會為1
	li $t7,0			#重設旗標t7
	addi $s4,$s4,1			#因為多走了這部，所以要將插值調整回去，讓hx,hy,foodx,foody的距離差正確
	li $t6,15			#用於判斷是否進入死亡迴圈
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
	beq $t1,$t0,Ls3			# 若下一步會吃到自己，則轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls3			# 若下一步會出界，則轉向
	beq $t7,$0,judgeS		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若又則t7會為1
	li $t7,0			#重設旗標t7
	addi $s4,$s4,-1			#因為多走了這部，所以要將插值調整回去，讓hx,hy,foodx,foody的距離差正確
	li $t6,15			#用於判斷是否進入死亡迴圈
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
	beq $t1,$t0,Ls4			# 若下一步會吃到自己，則轉向
	lw $t0,contourcolor		#load contour color
	beq $t1,$t0,Ls4			# 若下一步會出界，則轉向
	beq $t7,$0,judgeS		# t7 是一個旗標，用於確認是否已經因為下一步會吃到自己而轉向走其他方向，若又則t7會為1
	li $t7,0			#重設旗標t7
	addi $s5,$s5,-1			#因為多走了這部，所以要將插值調整回去，讓hx,hy,foodx,foody的距離差正確
	li $t6,15			#用於判斷是否進入死亡迴圈
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
