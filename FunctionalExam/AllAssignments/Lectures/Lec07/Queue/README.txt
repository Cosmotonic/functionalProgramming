A project structure is created to contain the source code and
compilation for libraries using the donet commandline approach.

1) Create solution

  % dotnet new sln -o QueueExample

2) Create simple Queue project

  % cd QueueExample

  % dotnet new classlib -lang "F#" -o src/QueueSimple

3) Make files QueueSimple.fsi, QueneSimple.fs and adjust project file

4) Add project to solution

  % dotnet sln add src/QueueSimple/QueueSimple.fsproj

5) Build solution

% dotnet build
  MSBuild version 17.9.4+90725d08d for .NET
    Determining projects to restore...
    All projects are up-to-date for restore.
    QueueSimple -> /Users/nh/.../src/QueueSimple/bin/Debug/net8.0/QueueSimple.dll

Build succeeded.
    0 Warning(s)
    0 Error(s)

Time Elapsed 00:00:00.79

6) Run example file QueueSimpleExample.fsx

  % dotnet fsi QueueSimpleExample.fsx 
  q0' = { front = []
    rear = [] }
  ...

7) Create project for QueueExample

  % dotnet new classlib -lang "F#" -o src/Queue

8) Add files Queue.fsi and Queue.fsi to the project file Queue.fsproj

  <ItemGroup>
    <Compile Include="Queue.fsi" />
    <Compile Include="Queue.fs" />    
  </ItemGroup>


9) Add project to solution

  % dotnet sln add src/Queue/Queue.fsproj

10) Build the solution

  % dotnet build
  MSBuild version 17.9.4+90725d08d for .NET
    Determining projects to restore...
    All projects are up-to-date for restore.
    QueueSimple -> /Users/nh/.../src/QueueSimple/bin/Debug/net8.0/QueueSimple.dll
    Queue -> /Users/nh/.../src/Queue/bin/Debug/net8.0/Queue.dll

  Build succeeded.
      0 Warning(s)
      0 Error(s)

11) Run the example QueueExample.fsx

  % dotnet fsi QueueExample.fsx 
  q0' = []
  q0 = []
  q1 = [1]
  q2 = [1; 2]
  (x,q3) = (1, [2])
  qnew = [2]
  qnew = q3 = true
  qnew < q3 = false (expected false)
  qnew > q3 = false (expected false)
  q1 < q2 = true (expected true)


