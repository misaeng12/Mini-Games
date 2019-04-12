install.packages("gWidgets")
install.packages("gWidgetsRGtk2")
library(gWidgets)
library(gWidgetsRGtk2)


win <- gwindow("Mini games")
grp <- ggroup(horizontal=FALSE, cont=win)
tmp <- gframe("Choose a game.", container=grp)



##########---------- 1. TicTacToe ----------##########

button1 <- gbutton("TicTacToe", handler=function(h,...){

 win1 <- gwindow("TicTacToe", width=1000)

 reset <-function(){
 X<<-matrix(0,3,3)
 return(X)
 }

 draw1 <- function() {
  plot<-plot(1, type='n', xlab='', ylab='',xaxt='n', yaxt='n',pos=0,bty='n',xlim=c(0,6),ylim=c(0,6))
  box(which = "figure", lty = "solid",col="red")
  text(2.25,2.95,"O",col="black",cex=4)
 }
 draw2 <- function() {
  plot<-plot(1, type='n', xlab='', ylab='',xaxt='n', yaxt='n',pos=0,bty='n',xlim=c(0,6),ylim=c(0,6))
  box(which = "figure", lty = "solid",col="red")
  text(2.25,2.95,"X",col="black",cex=4)
 }
 draw3 <- function() {
  plot<-plot(1, type='n', xlab='', ylab='',xaxt='n', yaxt='n',pos=0,bty='n',xlim=c(0,6),ylim=c(0,6))
  box(which = "figure", lty = "solid",col="red")
  text(2.25,2.95,"",col="black",cex=4)
 }
 draw <- function(i,j) {
  par(mfrow=c(3,3),bg="lightgreen")
  for (i in 1:3){
  for (j in 1:3){
   if (X[i,j]==1){
    draw1()
    } else if (X[i,j]==2){
    draw2()
    } else {
    draw3()
    }
   }
  }
 }
 firstdraw <- function(){
  par(mfrow=c(3,3),bg="lightgreen")
  k<-0
 repeat{
  plot<-plot(1, type='n', xlab='', ylab='',xaxt='n', yaxt='n',pos=0,bty='n',xlim=c(0,6),ylim=c(0,6))
  box(which = "figure", lty = "solid",col="red")
  text(2.25,2.75,"",adj=0.5,col="black",cex=4)
  k<-k+1
  if (k==9) break
  }
 }
 num<-function(x){
 a<<-as.numeric(strsplit(x,split="")[[1]][1])
 b<<-as.numeric(strsplit(x,split="")[[1]][2])
 }

 tictactoe1 <- function(i,j){
 if (X[i,j]!=0){
  gmessage("이 자리는 이미 플레이한 자리임")
 }
 X[i,j]<<-1
 for (n in 1:3){
   if (identical(X[n,],c(1,1,1))){
    gmessage("A wins")
    } else if (identical(X[,n],c(1,1,1))){
    gmessage("A wins")
    }
   }
   if (identical(diag(X),c(1,1,1))){
    gmessage("A wins")
    } else if(identical(diag(X[,3:1]),c(1,1,1))){
    gmessage("A wins")
    }
 return(X)
 }

 tictactoe2 <- function(i,j){
 if (X[i,j]!=0){
  gmessage("이 자리는 이미 플레이한 자리임")
 }
 X[i,j]<<-2
 for (n in 1:3){
   if (identical(X[n,],c(2,2,2))){
    gmessage("B wins")
    } else if (identical(X[,n],c(2,2,2))){
    gmessage("B wins")
    }
   }
   if (identical(diag(X),c(2,2,2))){
    gmessage("B wins")
    } else if(identical(diag(X[,3:1]),c(2,2,2))){
    gmessage("B wins")
    }
 return(X)
 }

 Aplay <- function(){
  svalue(type1)
  num(svalue(type1))
  tictactoe1(a,b)
  draw()
  galert("B's turn")
  svalue(type1) <<- "not your turn"
  svalue(type2) <<- ""
 }

 Bplay <- function(){
  svalue(type2)
  num(svalue(type2))
  tictactoe2(a,b)
  draw()
  galert("A's turn")
  svalue(type2) <<- "not your turn"
  svalue(type1) <<- ""
 }

 group <- ggroup(horizontal=FALSE, cont=win1)
  tmp1<<-gframe("Game", horizontal=FALSE,container=group ,cont=win1,expand=TRUE)
  group2<-ggroup(cont=tmp1)
  tframe<<-gframe("Type in i and j", container=group2, cont=tmp1, expand=TRUE)
  type1<<-gedit(initial.msg = "Player 1",cont=tframe)
  button1 <- gbutton("play",border=TRUE, cont=tframe,handler=function(h,...)Aplay())
  type2<<-gedit(initial.msg = "Player 2",cont=tframe)
  button2 <- gbutton("play",border=TRUE, cont=tframe,handler=function(h,...)Bplay())
  reset()
  add(tmp1,ggraphics())
  firstdraw()
  galert("A's turn")

}, container=grp, cont=tmp)



                     
                     
##########---------- 2. HangMan ----------##########
                     
button2 <- gbutton("HangMan", handler=function(h,...){

 win2 <- gwindow("HangMan")

 quiz <- function(x) {
  x1<<-unlist(strsplit(toupper(x),""))
  n<<-length(x1)
  y<<-rep("_",n)
  p<<-0
 }

 h0<-"C:/Temp/h0.gif"
 h1<-"C:/Temp/h1.gif"
 h2<-"C:/Temp/h2.gif"
 h3<-"C:/Temp/h3.gif"
 h4<-"C:/Temp/h4.gif"
 h5<-"C:/Temp/h5.gif"
 h6<-"C:/Temp/h6.gif"

 gp<-ggroup(horizontal=FALSE, cont=win2)
 tmp1<-gframe("Give a quiz", container=gp)          
 edit1<-gedit("", initial.msg="Input a word.", cont=tmp1)
 button1<-gbutton("enter", handler=function(h,...) {
  x<-svalue(edit1)
  svalue(edit1)<-""
  if(x=="") gmessage("Input a word!")  
  else {
   quiz(x)
   delete(tmp1,button1)
   img<-gimage(h0, container=gp)
   label1<-glabel(container=gp)          # word
   label2<-glabel(container=gp)          # message
   label3<-glabel(container=gp)          # lost points
   label4<-glabel(container=gp)          # number of letters
   svalue(label1)<-list(y)
   svalue(label4)<-paste("number of letters =",n)
   tmp2<-gframe("Try to guess", container=gp)          
   edit2<-gedit("", initial.msg="Input a letter", cont=tmp2)
   reset <- function() {
    delete(gp, img)
    delete(gp, label1)
    delete(gp, label2)
    delete(gp, label3)
    delete(gp, label4)
    delete(gp, tmp2)
    delete(gp, button2)
    add(tmp1, button1)
   }
   button2<-gbutton("RESET : Try new quiz", handler=function(h,...){
    reset()
   }, container=gp)
   gbutton("enter", handler=function(h,...) {
    a<-svalue(edit2)
    svalue(edit2)<-""
    A<-toupper(a)
    y.tmp<-y
    if(length(unlist(strsplit(a,"")))!=1) {
      gmessage("Input one letter")
    } else if(sum(rep(A,26)==LETTERS)==0) {
      gmessage("Input only letter")
    } else {
      for(i in 1:n) {
        if(A==x1[i]) {
          y[i]<<-A
          t<-sum(rep(A,n)==x1)
          svalue(label2)<-paste("This word contains","“",A,"”",t,"time(s)")
         }
        }
        svalue(label1)<-list(y)
        if(identical(y,y.tmp)) {
         p<<-p+1
         svalue(label2)<-paste("“",A,"”","is not in this word")
         svalue(label3)<-paste("lost points =",p)
        }
        if(p==1) svalue(img)<-h1
        if(p==2) svalue(img)<-h2
        if(p==3) svalue(img)<-h3
        if(p==4) svalue(img)<-h4
        if(p==5) svalue(img)<-h5
        if(p==6) {
         svalue(img)<-h6
         gmessage("failed!")
         reset()
        }
        if(identical(y,x1)) {
         gmessage("You win!")
         reset()
        }
       }
      }, cont=tmp2)
    }
  }, cont=tmp1)

}, container=grp, cont=tmp)



                     
                     
##########---------- 3. Matching ----------##########
                     
button3 <- gbutton("Matching", handler=function(h,...){

 win3<<-gwindow("Matching")

 pool<-rep(1:8,2)
  U<<-sample(pool)
  A<<-matrix(U,ncol=4,byrow=TRUE)
  point<<-0

 gp<<-ggroup(horizontal=FALSE,cont=win3)

 tmp<-gframe("Start",horizontal=FALSE,container=gp,cont=win3,expand=TRUE)
 gbutton("시작",border=TRUE,cont=tmp,function(h,...)plotting())
 cardchoice<<-gframe("카드 선택", horizontal=FALSE,container=gp,cont=win3,expand=TRUE)
 xyavail<<-c(1:16)
 card1<<-gcombobox(xyavail,cont=cardchoice)
 card2<<-gcombobox(xyavail,cont=cardchoice)
 gbutton("카드보기",border=TRUE,cont=cardchoice,function(h,...)cardshowing())
 gbutton("확인",border=TRUE, cont=cardchoice,function(h,...)game())

 tmp<-gframe("Score",container=gp,cont=win3,expand=TRUE)
 score1<<-glabel("",cont=tmp,expand=TRUE)

 tmp<-gframe("Result",container=gp,cont=win3, expand=TRUE)
 result1<<-glabel("",cont=tmp,expand=TRUE)

 tmp<-gframe("Record",container=gp,cont=win3,expand=TRUE)
 record1<<-glabel("",cont=tmp,expand=TRUE)

 add(win3,ggraphics())

 plotting<<-function(h,...){
  plot(0:4,0:4,type="n",xlab="",ylab="",axes=FALSE)
  abline(v=c(0:4),h=c(0:4),lty=3,col=1)
  x.axis<-c(1:4)-0.5
  y.axis<-rev(x.axis)
  t.num<-c(1:4)
  for (j in y.axis){
   for ( i in x.axis) {
    text( i, j,t.num[(i+0.5)])
    }
   t.num<-t.num+4
  }
 }

 record<<-rep(0,16)    

 point.card<-function(q){
  xa<-c(0.5,1.5,2.5,3.5)
  ya<-c(3.5,2.5,1.5,0.5)
  ima<<-as.numeric(c(15,16,17,18))
  X<-function(p) ifelse((p%%4==0),4,p%%4)
  Y<-function(p) ifelse((p%%4==0),as.integer(p/4),as.integer(p/4)+1)
  r<-U[q]
  col1<-ifelse(r<=4,2,3)
  xvalue<-xa[X(q)]
  yvalue<-ya[Y(q)]
  points(xvalue,yvalue,pch=ima[(r%%4)+1],cex=3,col=col1)
 }

 cardshowing<-function(h,...){
   a<<-svalue(card1)
   b<<-svalue(card2)
  if(is.na(match(a,record))==TRUE&is.na(match(b,record))==TRUE & a!=b){
    point.card(a)
    point.card(b)
   }
 }

 game <- function(h,...){
   a<<-svalue(card1)
   b<<-svalue(card2)
   x<-U[a]
   y<-U[b]
  if(is.na(match(a,record))==TRUE&is.na(match(b,record))==TRUE ){
  if(a==b){
   svalue(result1)<-""
   svalue(result1)<-"Choose different number!"
   }
  else{
  if(x==y){
    svalue(result1)<-""
    svalue(result1)<-"Correct!" 
    record[2*point+1]<<-a
    record[2*point+2]<<-b
    point<<-point+1
    svalue(score1)<-""
    svalue(score1)<-point
    svalue(record1)<-""
    svalue(record1)<-sort(record[record!=0])[1:(2*point)]
    plotting()
    }
  else if (x!=y){
    plotting()     
    svalue(result1)<-""
    svalue(result1)<-"Incorrect!"  
    point<<-point
    svalue(score1)<-""
    svalue(score1)<-point
   }  
  if(point==8) {
   svalue(result1)<-""
   svalue(result1)<-"CLEAR!"
  }
 }
 }
 }

}, container=grp, cont=tmp)
