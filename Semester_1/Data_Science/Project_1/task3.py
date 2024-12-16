# Initialize the parents class
class LungDisease:
    def __init__(self, name):
        self.name = name
        self.category = "Lung Disease"
    
    def get_info(self):
        return f"Disease Information: \n\nName: {self.name}\nCategory: {self.category}\n"

# First Child Class Infectious Lung Disease
class InfectiousDisease(LungDisease):
    def __init__(self, name, severity, pathogen_type, is_contagious):
        super().__init__(name)
        self.pathogen_type = pathogen_type
        self.is_contagious = is_contagious
        self.sub_category = "Infectious Disease"
        self.severity = severity
    
    def get_info(self):
        base_info = super().get_info()
        return f"{base_info}\nSub-category: {self.sub_category}\nPathogen Type: {self.pathogen_type}\n\
Severity : {self.severity}\n\
Contagious: {'Yes' if self.is_contagious else 'No'}"
    
    def get_pathogen_type(self):
        return f"{self.name} is a disease infected by {self.pathogen_type}"

# Second Child Class Non - Infectious Lung Disease
class NonInfectiousDisease(LungDisease):
    def __init__(self, name, severity, is_chronic):
        super().__init__(name)
        self.is_chronic = is_chronic
        self.sub_category = "Non-Infectious Disease"
        self.severity = severity
    
    def get_info(self):
        base_info = super().get_info()
        return f"{base_info}\nSub-category: {self.sub_category}\nSeverity : {self.severity}\n\
Chronic Condition: {'Yes' if self.is_chronic else 'No'}"
    
    def get_chronic_condition(self):
        return self.is_chronic

# Third Child Class Malignancy
class Malignancy(LungDisease):
    def __init__(self, name, severity, cancer_stage : int, metastasis : bool):
        super().__init__(name)
        self.cancer_stage = cancer_stage
        self.metastasis = metastasis
        self.severity = severity
        self.sub_category = "Malignancy"
    
    def get_info(self):
        base_info = super().get_info()
        return f"{base_info}\nSub-category: {self.sub_category}\nSeverity : {self.severity}\nCancer Stage: {self.cancer_stage}\n\
Metastasis: {'Present' if self.metastasis else 'Absent'}"
    
    def prognosis(self):
        return f"{self.name} is a stage {self.cancer_stage} cancer with {'metastasis' if self.metastasis else 'no metastasis'} and has a {'poor' if self.cancer_stage > 2 or self.metastasis == True else 'good'} prognosis"