; ModuleID = 'BURGer'
source_filename = "BURGer"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [3 x i8] c"%d\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.3 = private unnamed_addr constant [3 x i8] c"%d\00"
@str = private unnamed_addr constant [6 x i8] c"works\00"
@str.4 = private unnamed_addr constant [6 x i8] c"wrong\00"
@str.5 = private unnamed_addr constant [4 x i8] c"yes\00"
@str.6 = private unnamed_addr constant [3 x i8] c"no\00"
@str.7 = private unnamed_addr constant [4 x i8] c"yas\00"

declare i32 @print(i8*, ...)

declare i32 @printf(i8*, ...)

declare i32 @println(i8*, ...)

define i32 @main() {
entry:
  call void @add(i32 46, i32 32)
  ret i32 0
}

define void @add(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %b2 = alloca i32
  store i32 %b, i32* %b2
  %r = alloca i32
  %a3 = load i32, i32* %a1
  %b4 = load i32, i32* %b2
  %tmp = add i32 %a3, %b4
  store i32 %tmp, i32* %r
  %r5 = load i32, i32* %r
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.2, i32 0, i32 0), i32 %r5)
  %a6 = load i32, i32* %a1
  %b7 = load i32, i32* %b2
  %tmp8 = sub i32 %a6, %b7
  store i32 %tmp8, i32* %r
  %r9 = load i32, i32* %r
  %printf10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.2, i32 0, i32 0), i32 %r9)
  %a11 = load i32, i32* %a1
  %b12 = load i32, i32* %b2
  %tmp13 = mul i32 %a11, %b12
  store i32 %tmp13, i32* %r
  %r14 = load i32, i32* %r
  %printf15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.2, i32 0, i32 0), i32 %r14)
  %a16 = load i32, i32* %a1
  %b17 = load i32, i32* %b2
  %tmp18 = icmp sgt i32 %a16, %b17
  br i1 %tmp18, label %then, label %else

merge:                                            ; preds = %else, %then
  %a19 = load i32, i32* %a1
  %b20 = load i32, i32* %b2
  %tmp21 = icmp slt i32 %a19, %b20
  br i1 %tmp21, label %then23, label %else25

then:                                             ; preds = %entry
  %println = call i32 (i8*, ...) @println(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str, i32 0, i32 0))
  br label %merge

else:                                             ; preds = %entry
  br label %merge

merge22:                                          ; preds = %else25, %then23
  %a26 = load i32, i32* %a1
  %b27 = load i32, i32* %b2
  %tmp28 = icmp sge i32 %a26, %b27
  br i1 %tmp28, label %then30, label %else32

then23:                                           ; preds = %merge
  %println24 = call i32 (i8*, ...) @println(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str.4, i32 0, i32 0))
  br label %merge22

else25:                                           ; preds = %merge
  br label %merge22

merge29:                                          ; preds = %else32, %then30
  %a33 = load i32, i32* %a1
  %b34 = load i32, i32* %b2
  %tmp35 = icmp sle i32 %a33, %b34
  br i1 %tmp35, label %then37, label %else39

then30:                                           ; preds = %merge22
  %println31 = call i32 (i8*, ...) @println(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str.5, i32 0, i32 0))
  br label %merge29

else32:                                           ; preds = %merge22
  br label %merge29

merge36:                                          ; preds = %else39, %then37
  %a40 = load i32, i32* %a1
  %b41 = load i32, i32* %b2
  %tmp42 = icmp ne i32 %a40, %b41
  br i1 %tmp42, label %then44, label %else46

then37:                                           ; preds = %merge29
  %println38 = call i32 (i8*, ...) @println(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @str.6, i32 0, i32 0))
  br label %merge36

else39:                                           ; preds = %merge29
  br label %merge36

merge43:                                          ; preds = %else46, %then44
  ret void

then44:                                           ; preds = %merge36
  %println45 = call i32 (i8*, ...) @println(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str.7, i32 0, i32 0))
  br label %merge43

else46:                                           ; preds = %merge36
  br label %merge43
}
