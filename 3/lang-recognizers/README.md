# Simple language recognizers

Define recognizers for the following languages:
- $L_{0} = \\{ w \in \\{0,1\\}^* \mid \forall w'.\ \forall a \in \\{0,1\\}.\ w = w' a \Rightarrow a \neq 0 \\}$
- $L_1 = \\{ w \in \\{0,1\\}^* \mid \forall x,y.\ w = x 0 y \Rightarrow |y|_0 = 0 \\}$ 
- $L_2 = \\{ w \in \\{0,1,2\\}^* \mid \forall x,y.\ w = x 1 y \Rightarrow |y|_0 = 0 \\}$
- $L_3 = \\{ w \mid w = x 0 y \Rightarrow \exists z .\ y = 1 1 z \\}$
- $L_4 = \\{ w \in \\{0,1\\}^* \mid |w|_0 \geq |w|_1 \\}$
- $L_5 = \\{ w \in \\{0,1\\}^* \mid |w|_0 = |w|_1 \\}$
- $L_6 = \\{ 0^n 1^n \mid n \geq 0 \\}$
- $L_7 = \\{ 0^n 1 0^n \mid n \geq 0 \\}$
- $L_8 = \\{ w \in \\{0,1,2\\}^* \mid \forall x,y.\ w = x 1 y \Rightarrow |y|_2 = |y|_0 \\}$
- $L_9 = \\{ w \mid \exists w_0,w_1 .\ w = w_0 w_1 \land |w_0|_0 = |w_1|_1 \land |w_0|_1 = 0 = |w_1|_0 \\}$
