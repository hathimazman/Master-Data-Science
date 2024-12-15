# %%
def add(a,b):
    return a+b

def sub(a,b):
    return a-b

def mul(a,b):
    return a*b

def ayam(a):
    print(a)

# %%
def make_pizza(size,*toppings):
    """Summarize the pizza we are about to make"""
    print(f'\nMaking a {size}-inch pizza with the following toppings:')
    for topping in toppings:
        print(f"- {topping}")

# %%

