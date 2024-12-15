class Car():
    """A simple attempt to classify a car."""
    def __init__(self,make, model, year):
        """Initiallize attributes to describe a car."""
        self.make = make
        self.model = model
        self.year = year
        self.odometer_reading = 0                       #Set default value

    def get_descriptive_name(self):
        """Return a neatly formatted descriptive name."""
        long_name = f"{self.year} {self.make} {self.model}"
        return long_name.title()
    
    def read_odometer(self):
        """Print a statement showing the car's mileage."""
        print(f"This car has {self.odometer_reading} miles on it")

    def increment_odometer(self,miles):
        """Add given amount to current odometer reading."""
        self.odometer_reading += miles
        print(f"Your new odometer is {self.odometer_reading}")

class ElectricCar(Car):
    """Represent aspects of a car, specific to electric vehicles."""
    def __init__(self, make, model, year):
        """Initialize attributes of the parent class."""
        super().__init__(make, model, year)
        self.batter_size = 70

    def describe_battery(self):
        """Print a statemtn describing the battery size."""
        print(f"This car has a {self.batter_size}-kWh battery.")