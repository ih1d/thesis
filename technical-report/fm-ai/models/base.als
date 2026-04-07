/*
 * Author: Isaac H. Lopez Diaz
 * Description: AI Agent for QA of a code repo
 * An AI Agent is viewed as anything that can
 * perceive its own environment and act upon it. (AIMA 4th)
*/

module models/base

// We can view environments as the global state
abstract sig Environment {}

/* 
 * Task environments are the problems to which
 * (rational) agents are the solution.
 * Task environments have the following properties:
 * (1) Fully Observable vs Partially Observable
 * (2) Single Agent vs Multiagent
 * (3) Deterministic vs Nondeterministic
 * (4) Episodic vs Sequential
 * (5) Static vs Dynamic
 * (6) Discrete vs Continuous
 * (7) Known vs Unknown 
*/
abstract sig Property {}