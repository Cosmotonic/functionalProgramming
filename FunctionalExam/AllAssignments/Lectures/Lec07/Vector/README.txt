A project structure is created to contian the source code and
compilation for libraries using the donet commandline approach.

A simple manual is found here:
https://learn.microsoft.com/da-dk/dotnet/fsharp/get-started/get-started-command-line

1) Create Vector project solution

  % dotnet new sln -o VectorExample
  The template "Solution File" was created successfully.

2) Create Vector library project

  % cd VectorExample

  % dotnet new classlib -lang "F#" -o src/VectorSimple
  The template "Class Library" was created successfully.

  Processing post-creation actions...
  Restoring /Users/nh/Dropbox/Documents/Work/ITU/Course/KSFUPRO1KU-F2024/Lectures/Lec06/Vector/VectorExample/src/VectorSimple/VectorSimple.fsproj:
    Determining projects to restore...
    Restored /Users/nh/Dropbox/Documents/Work/ITU/Course/KSFUPRO1KU-F2024/Lectures/Lec06/Vector/VectorExample/src/VectorSimple/VectorSimple.fsproj (in 981 ms).
  Restore succeeded.

nh@MBP-Niels VectorExample % 

3) Add Vector library project to Vector solution.

% dotnet sln add src/VectorSimple/VectorSimple.fsproj
  Project `src/VectorSimple/VectorSimple.fsproj` added to the solution.

4) Add the VectorSimple implementation files to the VectorSimple project.

% ll src/VectorSimple 
-rw-r--r--@ 1 nh  staff  618 10 Mar 10:14 VectorSimple.fs
-rw-r--r--@ 1 nh  staff  712 10 Mar 10:14 VectorSimple.fsi
-rw-r--r--@ 1 nh  staff  276 10 Mar 10:11 VectorSimple.fsproj
...

5) Update VectorSimple.fsproj to contain the two files

  <ItemGroup>
    <Compile Include="VectorSimple.fsi" />    
    <Compile Include="VectorSimple.fs" />
  </ItemGroup>

I have removed reference to Library.fs as we do not use the file.

6) Build the library

  % dotnet build
  MSBuild version 17.9.4+90725d08d for .NET
    Determining projects to restore...
    All projects are up-to-date for restore.
    VectorSimple -> /Users/nh/...Vector/VectorExample/src/VectorSimple/bin/Debug/net8.0/VectorSimple.dll

  Build succeeded.
      0 Warning(s)
      0 Error(s)

  Time Elapsed 00:00:00.32

7) The compiled library is found here:

  % ll src/VectorSimple/bin/Debug/net8.0 
  total 48
  drwxr-xr-x@ 6 nh  staff    192 10 Mar 10:49 .
  drwxr-xr-x@ 3 nh  staff     96 10 Mar 10:14 ..
  -rw-r--r--@ 1 nh  staff   2385 10 Mar 10:49 VectorSimple.deps.json
  -rw-r--r--@ 1 nh  staff  11776 10 Mar 10:49 VectorSimple.dll
  -rw-r--r--@ 1 nh  staff   2224 10 Mar 10:49 VectorSimple.pdb

8) Now we can use the library

Place the file VectorSimpleExample.fsx in same directory as the
library files Vector.fsi and Vector.fs.

  % ll src/VectorSimple 
  total 48
  drwxr-xr-x@ 10 nh  staff  320 10 Mar 10:53 .
  drwxr-xr-x@  3 nh  staff   96 10 Mar 10:51 ..
  -rw-r--r--@  1 nh  staff  618 10 Mar 10:14 VectorSimple.fs
  -rw-r--r--@  1 nh  staff  712 10 Mar 10:14 VectorSimple.fsi
  -rw-r--r--@  1 nh  staff  329 10 Mar 10:49 VectorSimple.fsproj
  -rw-r--r--@  1 nh  staff  215 10 Mar 10:53 VectorSimpleExample.fsx
  drwxr-xr-x@  3 nh  staff   96 10 Mar 10:14 bin
  drwxr-xr-x@  8 nh  staff  256 10 Mar 10:14 obj

Make sure to reference VectorSimple.dll from VectorSimpleExample.fsx,
e.g., depending on your current directory.

 % pwd
  /Users/nh/.../Vector/VectorExample/src/VectorSimple


  #r @"./bin/Debug/net8.0/VectorSimple.dll"

You can then execute script line by line in VSCode or VS.

You can also run the entire script using dotnet. Again you need to
adjust reference as of current directory.

 % dotnet fsi VectorSimpleExample.fsx
  a = V (1.0, -2.0)
  b = V (3.0, 4.0)
  c = V (-1.0, -8.0)
  coord c = (-1.0, -8.0)
  d = 15.0
  e = 5.0


*************

Now adding example on Type Extension

1) Add class library VectorTypeExtension

  % dotnet new classlib -lang "F#" -o src/VectorTypeExtension
  The template "Class Library" was created successfully.

  Processing post-creation actions...
  Restoring /Users/nh/.../src/VectorTypeExtension/VectorTypeExtension.fsproj:
    Determining projects to restore...
    Restored /Users/nh/.../src/VectorTypeExtension/VectorTypeExtension.fsproj (in 417 ms).
  Restore succeeded.


2) Add project to Vector solution.

  % dotnet sln add src/VectorTypeExtension/VectorTypeExtension.fsproj
  Project `src/VectorTypeExtension/VectorTypeExtension.fsproj` added to the solution.

3) Add VectorTypeExtension.fs and Vector.fsi to the directory and
update VectorTypeExtension.fsprog accordingly.

  <ItemGroup>
    <Compile Include="Vector.fsi" />    
    <Compile Include="VectorTypeExtension.fs" />
  </ItemGroup>

  % ll src/VectorTypeExtension 
-rw-r--r--@ 1 nh  staff  714 10 Mar 12:11 VectorTypeExtension.fs
-rw-r--r--@ 1 nh  staff  583 10 Mar 12:11 Vector.fsi
-rw-r--r--@ 1 nh  staff  317 10 Mar 12:12 VectorTypeExtension.fsproj

4) Build everything with dotnet build.

Above was continued until we had the file structure you find in the
VectorExample folder containing three examples: VectorSimple,
VectorTypeAugmentation and VectorTypeExtension.
