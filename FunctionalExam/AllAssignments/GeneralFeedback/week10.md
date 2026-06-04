# General Feedback for Week 10
All of you did great on the questions of the exam set, so well done!!

## Question 10.3

There seemed to be a little confusion on how to approach this exercise. 
Please remember to include an explanation of your solutions. 

For instance, instead of just pointing out the fix, you might include an explanation like this: 

The memory leak occurs because of the helper pointer 'first' in the Get() function in SentinelLockQueue. When a node is dequeued from the queue, head is set to point to the next node in the queue with an intermediate helper pointer, 'first', which is used to move head forward, so to speak. The problem arises because the old node is still pointed to by 'first' even after 'head' has been updated. Therefore the old node can technically still be reached and is not given up for garbage collection.

To fix the problem, we could either set the 'first' pointer to
'None' before the termination of the function or we could simply
skip the helper pointer and directly assign 'head' to be the
node that is currently its own successor.