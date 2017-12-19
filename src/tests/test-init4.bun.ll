; ModuleID = 'BURGer'
source_filename = "BURGer"

@s = global i0 0
@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [3 x i8] c"%d\00"

declare i32 @print(i8*, ...)

declare i32 @printf(i8*, ...)

declare i32 @println(i8*, ...)

define i32 @main() {
entry:
  %s = load i0, i0* @s
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @fmt.1, i32 0, i32 0), i0 %s)
  ret i32 0
}
