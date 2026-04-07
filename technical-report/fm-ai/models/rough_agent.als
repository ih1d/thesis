/* 
 * Author: Isaac H. Lopez Diaz
 * Description: First rough sketch of the AI Agent
*/

module models/rough_agent

sig Tool {}

sig Operation {}

/* 
 * We define AI agents as the following:
 * an agent is a program that has a set of
 * tools it can use, and the purpose the set
 * of operations it carries out.
*/
sig Agent {
    tools: set Tool,
    purpose: set Operation
}