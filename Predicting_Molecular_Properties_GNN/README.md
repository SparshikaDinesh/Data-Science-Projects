# Predicting Molecular Properties using Graph Neural Networks (GNNs)

**Course:** Advanced Machine Learning  
**Tech Stack:** Python, PyTorch Geometric, Deep Graph Library (DGL), RDKit  

### Overview
This project implements Graph Neural Networks (GNNs) to predict molecular properties by learning from molecular graph structures.  
Each molecule is represented as a graph, where atoms are nodes and bonds are edges. The model learns chemical representations directly from the graph data, improving prediction accuracy for molecular attributes such as solubility and toxicity.

### Key Features
- Preprocessing molecules using **RDKit** to generate graph-based structures  
- Implementation of **GCN**, **GraphSAGE**, and **GAT** models using **PyTorch Geometric**  
- Model optimization with **Adam optimizer** and **learning rate scheduling**  
- Achieved high correlation between predicted and actual property values  

### Files
- `GNN_Molecular_Properties.ipynb` – Full model implementation and training  
- `GNN_Molecular_Report.pdf` – Project report and performance analysis  
