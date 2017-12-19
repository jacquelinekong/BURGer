; ModuleID = 'BURGer'
source_filename = "BURGer"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [3 x i8] c"%d\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.3 = private unnamed_addr constant [3 x i8] c"%d\00"
@str = private unnamed_addr constant [3 x i8] c"ya\00"

declare i32 @print(i8*, ...)

declare i32 @printf(i8*, ...)

declare i32 @println(i8*, ...)

define i32 @main() {
entry:
  ret i32 0
}

define void @test() {
entry:
  %print = call i32 (i8*, ...) @print(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @str, i32 0, i32 0))
  ret void
}
