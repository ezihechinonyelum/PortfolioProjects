-- Data inspection
SELECT *
FROM [portfolio project]..CovidDeaths
ORDER BY date, location;

SELECT *
FROM [portfolio project]..CovidVaccinations
ORDER BY date, location;

SELECT location, date, total_cases, new_cases, total_deaths, population
FROM [portfolio project]..CovidDeaths
WHERE continent IS NOT NULL
ORDER BY date, location;

-- Calculate global total deaths and total cases
SELECT SUM(CAST(total_deaths AS int)) AS GlobalTotalDeaths, SUM(total_cases) AS GlobalTotalCases
FROM [portfolio project]..CovidDeaths;

-- Calculate death rate globally
SELECT location, (SUM(CAST(total_deaths AS int)) / SUM(total_cases)) * 100 AS DeathRate
FROM [portfolio project]..CovidDeaths
WHERE continent IS NOT NULL
GROUP BY location
ORDER BY location;

-- Calculate infection rate globally
SELECT location, SUM(total_cases) AS TotalCases, (SUM(total_cases) / population) * 100 AS InfectionRate
FROM [portfolio project]..CovidDeaths
WHERE continent IS NOT NULL
GROUP BY location, population
ORDER BY location;

-- Calculate daily new cases and deaths globally
SELECT date, SUM(CAST(new_cases AS int)) AS DailyNewCases, SUM(CAST(new_deaths AS int)) AS DailyNewDeaths
FROM [portfolio project]..CovidDeaths
WHERE continent IS NOT NULL
GROUP BY date
ORDER BY date;

-- Fetch countries with the highest infection rate
SELECT location, population, MAX(total_cases) AS HighestInfectionCount, MAX(total_cases / population) * 100 AS infection_rate
FROM [portfolio project]..CovidDeaths
WHERE continent IS NOT NULL
GROUP BY location, population
ORDER BY infection_rate DESC;

-- Fetch countries with the highest death count per population
SELECT location, MAX(CAST(total_deaths AS int)) AS TotalDeathCount
FROM [portfolio project]..CovidDeaths
WHERE continent IS NOT NULL
GROUP BY location
ORDER BY TotalDeathCount DESC;

-- Fetch continent with the highest death count per population
SELECT continent, MAX(CAST(total_deaths AS int)) AS TotalDeathCount
FROM [portfolio project]..CovidDeaths
WHERE continent IS NOT NULL
GROUP BY continent
ORDER BY TotalDeathCount DESC;

-- Join CovidDeaths and CovidVaccinations tables and calculate total population vs vaccination
SELECT cd.continent, cd.location, cd.date, cd.population, cv.new_vaccinations,
       SUM(CAST(cv.new_vaccinations AS int)) OVER (PARTITION BY cd.location ORDER BY cd.location, cd.date) AS RollingPeopleVaccinated
FROM [portfolio project]..CovidDeaths cd
JOIN [portfolio project]..CovidVaccinations cv ON cd.location = cv.location AND cd.date = cv.date
WHERE cd.continent IS NOT NULL
ORDER BY cd.location, cd.date;

-- Using CTE to calculate rolling vaccination percentage
WITH PopvsVac AS (
    SELECT cd.continent, cd.location, cd.date, cd.population, cv.new_vaccinations,
           SUM(CAST(cv.new_vaccinations AS int)) OVER (PARTITION BY cd.location ORDER BY cd.location, cd.date) AS RollingPeopleVaccinated
    FROM [portfolio project]..CovidDeaths cd
    JOIN [portfolio project]..CovidVaccinations cv ON cd.location = cv.location AND cd.date = cv.date
    WHERE cd.continent IS NOT NULL
)
SELECT *, (RollingPeopleVaccinated / Population) * 100 AS PercentVaccinated
FROM PopvsVac;

-- Create a temporary table to store population and vaccination data
DROP TABLE IF EXISTS #PercentPopulationVaccinated;
CREATE TABLE #PercentPopulationVaccinated (
    Continent NVARCHAR(200),
    Location NVARCHAR(200),
    Date DATETIME,
    Population NUMERIC,
    New_vaccinations NUMERIC,
    RollingPeopleVaccinated NUMERIC
);
INSERT INTO #PercentPopulationVaccinated
SELECT cd.continent, cd.location, cd.date, cd.population, cv.new_vaccinations,
       SUM(CAST(cv.new_vaccinations AS int)) OVER (PARTITION BY cd.location ORDER BY cd.location, cd.date) AS RollingPeopleVaccinated
FROM [portfolio project]..CovidDeaths cd
JOIN [portfolio project]..CovidVaccinations cv ON cd.location = cv.location AND cd.date = cv.date;

-- Calculate vaccination percentage
SELECT *, (RollingPeopleVaccinated / Population) * 100 AS PercentVaccinated
FROM #PercentPopulationVaccinated;

-- Create a view to store data for later (PercentPopulationVaccinated)
CREATE VIEW PercentPopulationVaccinated AS
SELECT cd.continent, cd.location, cd.date, cd.population, cv.new_vaccinations,
       SUM(CAST(cv.new_vaccinations AS int)) OVER (PARTITION BY cd.location ORDER BY cd.location, cd.date) AS RollingPeopleVaccinated
FROM [portfolio project]..CovidDeaths cd
JOIN [portfolio project]..CovidVaccinations cv ON cd.location = cv.location AND cd.date = cv.date
WHERE cd.continent IS NOT NULL;


CREATE VIEW DailyNewcasesandDeaths AS
SELECT date, SUM(CAST(new_cases AS int)) AS DailyNewCases, SUM(CAST(new_deaths AS int)) AS DailyNewDeaths
FROM [portfolio project]..CovidDeaths
WHERE continent IS NOT NULL
GROUP BY date;


CREATE VIEW DeathRate AS
SELECT location, (SUM(CAST(total_deaths AS int)) / SUM(total_cases)) * 100 AS DeathRate
FROM [portfolio project]..CovidDeaths
WHERE continent IS NOT NULL
GROUP BY location;


CREATE VIEW InfectinRate AS
SELECT location, SUM(total_cases) AS TotalCases, (SUM(total_cases) / population) * 100 AS InfectionRate
FROM [portfolio project]..CovidDeaths
WHERE continent IS NOT NULL
GROUP BY location, population;
