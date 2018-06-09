(ns periodic.table)

(def table'
  #js[{:letter "H", :name "Hydrogen", :weight "1.00794", :col 1, :row 1, :i 0}
      {:letter "He", :name "Helium", :weight "4.002602", :col 18, :row 1, :i 1}
      {:letter "Li", :name "Lithium", :weight "6.941", :col 1, :row 2, :i 2}
      {:letter "Be", :name "Beryllium", :weight "9.012182", :col 2, :row 2, :i 3}
      {:letter "B", :name "Boron", :weight "10.811", :col 13, :row 2, :i 4}
      {:letter "C", :name "Carbon", :weight "12.0107", :col 14, :row 2, :i 5}
      {:letter "N", :name "Nitrogen", :weight "14.0067", :col 15, :row 2, :i 6}
      {:letter "O", :name "Oxygen", :weight "15.9994", :col 16, :row 2, :i 7}
      {:letter "F", :name "Fluorine", :weight "18.9984032", :col 17, :row 2, :i 8}
      {:letter "Ne", :name "Neon", :weight "20.1797", :col 18, :row 2, :i 9}
      {:letter "Na", :name "Sodium", :weight "22.98976...", :col 1, :row 3, :i 10}
      {:letter "Mg", :name "Magnesium", :weight "24.305", :col 2, :row 3, :i 11}
      {:letter "Al", :name "Aluminium", :weight "26.9815386", :col 13, :row 3, :i 12}
      {:letter "Si", :name "Silicon", :weight "28.0855", :col 14, :row 3, :i 13}
      {:letter "P", :name "Phosphorus", :weight "30.973762", :col 15, :row 3, :i 14}
      {:letter "S", :name "Sulfur", :weight "32.065", :col 16, :row 3, :i 15}
      {:letter "Cl", :name "Chlorine", :weight "35.453", :col 17, :row 3, :i 16}
      {:letter "Ar", :name "Argon", :weight "39.948", :col 18, :row 3, :i 17}
      {:letter "K", :name "Potassium", :weight "39.948", :col 1, :row 4, :i 18}
      {:letter "Ca", :name "Calcium", :weight "40.078", :col 2, :row 4, :i 19}
      {:letter "Sc", :name "Scandium", :weight "44.955912", :col 3, :row 4, :i 20}
      {:letter "Ti", :name "Titanium", :weight "47.867", :col 4, :row 4, :i 21}
      {:letter "V", :name "Vanadium", :weight "50.9415", :col 5, :row 4, :i 22}
      {:letter "Cr", :name "Chromium", :weight "51.9961", :col 6, :row 4, :i 23}
      {:letter "Mn", :name "Manganese", :weight "54.938045", :col 7, :row 4, :i 24}
      {:letter "Fe", :name "Iron", :weight "55.845", :col 8, :row 4, :i 25}
      {:letter "Co", :name "Cobalt", :weight "58.933195", :col 9, :row 4, :i 26}
      {:letter "Ni", :name "Nickel", :weight "58.6934", :col 10, :row 4, :i 27}
      {:letter "Cu", :name "Copper", :weight "63.546", :col 11, :row 4, :i 28}
      {:letter "Zn", :name "Zinc", :weight "65.38", :col 12, :row 4, :i 29}
      {:letter "Ga", :name "Gallium", :weight "69.723", :col 13, :row 4, :i 30}
      {:letter "Ge", :name "Germanium", :weight "72.63", :col 14, :row 4, :i 31}
      {:letter "As", :name "Arsenic", :weight "74.9216", :col 15, :row 4, :i 32}
      {:letter "Se", :name "Selenium", :weight "78.96", :col 16, :row 4, :i 33}
      {:letter "Br", :name "Bromine", :weight "79.904", :col 17, :row 4, :i 34}
      {:letter "Kr", :name "Krypton", :weight "83.798", :col 18, :row 4, :i 35}
      {:letter "Rb", :name "Rubidium", :weight "85.4678", :col 1, :row 5, :i 36}
      {:letter "Sr", :name "Strontium", :weight "87.62", :col 2, :row 5, :i 37}
      {:letter "Y", :name "Yttrium", :weight "88.90585", :col 3, :row 5, :i 38}
      {:letter "Zr", :name "Zirconium", :weight "91.224", :col 4, :row 5, :i 39}
      {:letter "Nb", :name "Niobium", :weight "92.90628", :col 5, :row 5, :i 40}
      {:letter "Mo", :name "Molybdenum", :weight "95.96", :col 6, :row 5, :i 41}
      {:letter "Tc", :name "Technetium", :weight "(98)", :col 7, :row 5, :i 42}
      {:letter "Ru", :name "Ruthenium", :weight "101.07", :col 8, :row 5, :i 43}
      {:letter "Rh", :name "Rhodium", :weight "102.9055", :col 9, :row 5, :i 44}
      {:letter "Pd", :name "Palladium", :weight "106.42", :col 10, :row 5, :i 45}
      {:letter "Ag", :name "Silver", :weight "107.8682", :col 11, :row 5, :i 46}
      {:letter "Cd", :name "Cadmium", :weight "112.411", :col 12, :row 5, :i 47}
      {:letter "In", :name "Indium", :weight "114.818", :col 13, :row 5, :i 48}
      {:letter "Sn", :name "Tin", :weight "118.71", :col 14, :row 5, :i 49}
      {:letter "Sb", :name "Antimony", :weight "121.76", :col 15, :row 5, :i 50}
      {:letter "Te", :name "Tellurium", :weight "127.6", :col 16, :row 5, :i 51}
      {:letter "I", :name "Iodine", :weight "126.90447", :col 17, :row 5, :i 52}
      {:letter "Xe", :name "Xenon", :weight "131.293", :col 18, :row 5, :i 53}
      {:letter "Cs", :name "Caesium", :weight "132.9054", :col 1, :row 6, :i 54}
      {:letter "Ba", :name "Barium", :weight "132.9054", :col 2, :row 6, :i 55}
      {:letter "La", :name "Lanthanum", :weight "138.90547", :col 4, :row 9, :i 56}
      {:letter "Ce", :name "Cerium", :weight "140.116", :col 5, :row 9, :i 57}
      {:letter "Pr", :name "Praseodymium", :weight "140.90765", :col 6, :row 9, :i 58}
      {:letter "Nd", :name "Neodymium", :weight "144.242", :col 7, :row 9, :i 59}
      {:letter "Pm", :name "Promethium", :weight "(145)", :col 8, :row 9, :i 60}
      {:letter "Sm", :name "Samarium", :weight "150.36", :col 9, :row 9, :i 61}
      {:letter "Eu", :name "Europium", :weight "151.964", :col 10, :row 9, :i 62}
      {:letter "Gd", :name "Gadolinium", :weight "157.25", :col 11, :row 9, :i 63}
      {:letter "Tb", :name "Terbium", :weight "158.92535", :col 12, :row 9, :i 64}
      {:letter "Dy", :name "Dysprosium", :weight "162.5", :col 13, :row 9, :i 65}
      {:letter "Ho", :name "Holmium", :weight "164.93032", :col 14, :row 9, :i 66}
      {:letter "Er", :name "Erbium", :weight "167.259", :col 15, :row 9, :i 67}
      {:letter "Tm", :name "Thulium", :weight "168.93421", :col 16, :row 9, :i 68}
      {:letter "Yb", :name "Ytterbium", :weight "173.054", :col 17, :row 9, :i 69}
      {:letter "Lu", :name "Lutetium", :weight "174.9668", :col 18, :row 9, :i 70}
      {:letter "Hf", :name "Hafnium", :weight "178.49", :col 4, :row 6, :i 71}
      {:letter "Ta", :name "Tantalum", :weight "180.94788", :col 5, :row 6, :i 72}
      {:letter "W", :name "Tungsten", :weight "183.84", :col 6, :row 6, :i 73}
      {:letter "Re", :name "Rhenium", :weight "186.207", :col 7, :row 6, :i 74}
      {:letter "Os", :name "Osmium", :weight "190.23", :col 8, :row 6, :i 75}
      {:letter "Ir", :name "Iridium", :weight "192.217", :col 9, :row 6, :i 76}
      {:letter "Pt", :name "Platinum", :weight "195.084", :col 10, :row 6, :i 77}
      {:letter "Au", :name "Gold", :weight "196.966569", :col 11, :row 6, :i 78}
      {:letter "Hg", :name "Mercury", :weight "200.59", :col 12, :row 6, :i 79}
      {:letter "Tl", :name "Thallium", :weight "204.3833", :col 13, :row 6, :i 80}
      {:letter "Pb", :name "Lead", :weight "207.2", :col 14, :row 6, :i 81}
      {:letter "Bi", :name "Bismuth", :weight "208.9804", :col 15, :row 6, :i 82}
      {:letter "Po", :name "Polonium", :weight "(209)", :col 16, :row 6, :i 83}
      {:letter "At", :name "Astatine", :weight "(210)", :col 17, :row 6, :i 84}
      {:letter "Rn", :name "Radon", :weight "(222)", :col 18, :row 6, :i 85}
      {:letter "Fr", :name "Francium", :weight "(223)", :col 1, :row 7, :i 86}
      {:letter "Ra", :name "Radium", :weight "(226)", :col 2, :row 7, :i 87}
      {:letter "Ac", :name "Actinium", :weight "(227)", :col 4, :row 10, :i 88}
      {:letter "Th", :name "Thorium", :weight "232.03806", :col 5, :row 10, :i 89}
      {:letter "Pa", :name "Protactinium", :weight "231.0588", :col 6, :row 10, :i 90}
      {:letter "U", :name "Uranium", :weight "238.02891", :col 7, :row 10, :i 91}
      {:letter "Np", :name "Neptunium", :weight "(237)", :col 8, :row 10, :i 92}
      {:letter "Pu", :name "Plutonium", :weight "(244)", :col 9, :row 10, :i 93}
      {:letter "Am", :name "Americium", :weight "(243)", :col 10, :row 10, :i 94}
      {:letter "Cm", :name "Curium", :weight "(247)", :col 11, :row 10, :i 95}
      {:letter "Bk", :name "Berkelium", :weight "(247)", :col 12, :row 10, :i 96}
      {:letter "Cf", :name "Californium", :weight "(251)", :col 13, :row 10, :i 97}
      {:letter "Es", :name "Einstenium", :weight "(252)", :col 14, :row 10, :i 98}
      {:letter "Fm", :name "Fermium", :weight "(257)", :col 15, :row 10, :i 99}
      {:letter "Md", :name "Mendelevium", :weight "(258)", :col 16, :row 10, :i 100}
      {:letter "No", :name "Nobelium", :weight "(259)", :col 17, :row 10, :i 101}
      {:letter "Lr", :name "Lawrencium", :weight "(262)", :col 18, :row 10, :i 102}
      {:letter "Rf", :name "Rutherfordium", :weight "(267)", :col 4, :row 7, :i 103}
      {:letter "Db", :name "Dubnium", :weight "(268)", :col 5, :row 7, :i 104}
      {:letter "Sg", :name "Seaborgium", :weight "(271)", :col 6, :row 7, :i 105}
      {:letter "Bh", :name "Bohrium", :weight "(272)", :col 7, :row 7, :i 106}
      {:letter "Hs", :name "Hassium", :weight "(270)", :col 8, :row 7, :i 107}
      {:letter "Mt", :name "Meitnerium", :weight "(276)", :col 9, :row 7, :i 108}
      {:letter "Ds", :name "Darmstadium", :weight "(281)", :col 10, :row 7, :i 109}
      {:letter "Rg", :name "Roentgenium", :weight "(280)", :col 11, :row 7, :i 110}
      {:letter "Cn", :name "Copernicium", :weight "(285)", :col 12, :row 7, :i 111}
      {:letter "Nh", :name "Nihonium", :weight "(286)", :col 13, :row 7, :i 112}
      {:letter "Fl", :name "Flerovium", :weight "(289)", :col 14, :row 7, :i 113}
      {:letter "Mc", :name "Moscovium", :weight "(290)", :col 15, :row 7, :i 114}
      {:letter "Lv", :name "Livermorium", :weight "(293)", :col 16, :row 7, :i 115}
      {:letter "Ts", :name "Tennessine", :weight "(294)", :col 17, :row 7, :i 116}
      {:letter "Og", :name "Oganesson", :weight "(294)", :col 18, :row 7, :i 117}])


(def laths (into #{} (range 57 72)))
(def acts (into #{} (range 89 104)))

(defn table-group [i]
  (condp some [i]
    #{3 11 19 37 55 87} :alkali-metals
    #{4 12 20 38 56 88} :alkaline-earth-metals
    #{5 14 32 33 51 52} :metalloids
    #{13 31 49 50 81 82 83 84} :post-transition-metals
    #{6 7 8 15 16 34} :non-metals
    #{9 17 35 53 85} :halogens
    #{2 10 18 36 54 86} :noble-gases
    #{109 110 11 113 114 115 116 117 118} :unknown
    laths  :lathanides
    acts  :actinides
    :transition-metals))

(def colors
  {:blue "0,127,127"
   :bright-blue "0,180,255"
   :yellow "255,255,0"
   :purple "127,0,127"
   :violet "238,130,238"
   :green "0,127,0"
   :bright-green "0,255,0"
   :red "127,0,0"
   :bright-red "204,0,0"
   :orange "255, 165, 0"
   :pinkish "255,0,95"
   })

(def group->color
  {:alkali-metals :pinkish
   :alkaline-earth-metals :yellow
   :non-metals :bright-green
   :transition-metals :blue
   :actinides :green
   :lathanides :bright-red
   :halogens :bright-blue
   :noble-gases :purple
   :post-transition-metals :violet
   :metalloids :orange})

(defn xf [{:keys [i col row] :as e}]
  (let [i (inc i)
        group (table-group i)
        color (get colors (get group->color group) "255,255,255")]
    (assoc e :group group :color color :pos [col row] :i i)))

(def table (mapv xf table'))

