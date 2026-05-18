# ============================================================
# app.py
# Minneapolis Neighborhood Investment Model — Streamlit App
# ============================================================

import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import anthropic
import requests
import os
import time
from dotenv import load_dotenv

load_dotenv()

# ============================================================
# PAGE CONFIGURATION
# ============================================================
st.set_page_config(
    page_title="Minneapolis Investment Model",
    page_icon="📈",
    layout="wide",
    initial_sidebar_state="expanded"
)

# ============================================================
# CUSTOM CSS
# ============================================================
st.markdown("""
<style>
    /* ---- Force light theme regardless of OS/browser dark mode ---- */
    html, body, [data-testid="stAppViewContainer"],
    [data-testid="stHeader"], [data-testid="stToolbar"] {
        color-scheme: light !important;
    }

    /* ---- Global ---- */
    .main { background: #F4F5F7 !important; }
    [data-testid="stSidebar"] { background: #FFFFFF !important; border-right: 0.5px solid #E5E7EB !important; }

    /* ---- Header ---- */
    .main-header {
        background: #0f1e3d !important;
        padding: 1.25rem 1.75rem;
        border-radius: 10px;
        margin-bottom: 1rem;
        display: flex;
        align-items: center;
        justify-content: space-between;
    }
    .main-header h1 {
        color: #fff !important;
        font-size: 1.25rem;
        font-weight: 600;
        margin: 0;
        letter-spacing: -0.3px;
    }
    .main-header p {
        color: rgba(255,255,255,0.45) !important;
        margin: 0.25rem 0 0 0;
        font-size: 0.72rem;
        letter-spacing: 0.1px;
    }

    /* ---- KPI strip ---- */
    .kpi-strip {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 10px;
        margin-bottom: 1rem;
    }
    .kpi-card {
        background: #FFFFFF !important;
        border: 0.5px solid #E5E7EB !important;
        border-radius: 10px;
        padding: 12px 16px;
    }
    .kpi-label {
        font-size: 0.68rem;
        color: #9CA3AF !important;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 0.5px;
    }
    .kpi-value {
        font-size: 1.4rem;
        font-weight: 600;
        color: #111827 !important;
        margin-top: 3px;
        line-height: 1;
    }
    .kpi-sub {
        font-size: 0.68rem;
        color: #9CA3AF !important;
        margin-top: 3px;
    }

    /* ---- Metric cards ---- */
    .metric-card {
        background: #F9FAFB !important;
        border-radius: 8px;
        padding: 10px 12px;
        margin-bottom: 8px;
        border: none !important;
    }
    .metric-label {
        font-size: 0.68rem;
        color: #9CA3AF !important;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 3px;
    }
    .metric-value {
        font-size: 1.3rem;
        font-weight: 600;
        color: #111827 !important;
        line-height: 1.1;
    }
    .metric-sub {
        font-size: 0.68rem;
        color: #9CA3AF !important;
        margin-top: 2px;
    }

    /* ---- Tier badges ---- */
    .tier-badge {
        display: inline-flex;
        align-items: center;
        gap: 5px;
        padding: 3px 10px;
        border-radius: 20px;
        font-size: 0.72rem;
        font-weight: 600;
        margin-top: 4px;
    }
    .tier-dot { width: 6px; height: 6px; border-radius: 50%; }
    .tier-high     { background: #D1FAE5 !important; color: #065F46 !important; }
    .tier-moderate { background: #FEF3C7 !important; color: #92400E !important; }
    .tier-watch    { background: #FED7AA !important; color: #9A3412 !important; }
    .tier-low      { background: #FEE2E2 !important; color: #991B1B !important; }
    .tier-na       { background: #F3F4F6 !important; color: #6B7280 !important; }
    .dot-high     { background: #10B981 !important; }
    .dot-moderate { background: #F59E0B !important; }
    .dot-watch    { background: #F97316 !important; }
    .dot-low      { background: #EF4444 !important; }
    .dot-na       { background: #9CA3AF !important; }

    /* ---- Score bar ---- */
    .score-wrap { margin: 12px 0; }
    .score-row  { display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 5px; }
    .score-lbl  { font-size: 0.72rem; color: #9CA3AF !important; }
    .score-num  { font-size: 1.5rem; font-weight: 600; color: #111827 !important; }
    .score-track { height: 4px; background: #E5E7EB !important; border-radius: 2px; }
    .score-fill  { height: 4px; border-radius: 2px;
        background: linear-gradient(90deg, #ef4444 0%, #f59e0b 50%, #22c55e 100%) !important; }
    .score-sub  { font-size: 0.65rem; color: #9CA3AF !important; margin-top: 4px; }

    /* ---- AI box ---- */
    .ai-box {
        background: #EFF6FF !important;
        border: 0.5px solid #BFDBFE !important;
        border-radius: 8px;
        padding: 12px 14px;
        margin-top: 10px;
    }
    .ai-box-header {
        font-size: 0.65rem;
        font-weight: 600;
        color: #1D4ED8 !important;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 6px;
    }
    .ai-box-text {
        font-size: 0.82rem;
        color: #1E40AF !important;
        line-height: 1.65;
    }

    /* ---- Validation box ---- */
    .val-box {
        background: #F0FDF4 !important;
        border: 0.5px solid #BBF7D0 !important;
        border-radius: 8px;
        padding: 15px;
        margin-top: 20px;
    }
    .val-header { color: #166534 !important; font-weight: 600; margin-bottom: 10px; }

    /* ---- Disclaimer ---- */
    .disclaimer {
        font-size: 0.65rem;
        color: #9CA3AF !important;
        text-align: center;
        margin-top: 1rem;
        line-height: 1.6;
    }

    /* ---- Dark-mode hard reset ---- */
    @media (prefers-color-scheme: dark) {
        .kpi-card, .metric-card, .ai-box, .val-box,
        .main-header, .tier-badge, .score-track { filter: none !important; }
    }

    #MainMenu  { visibility: hidden; }
    footer     { visibility: hidden; }
</style>
""", unsafe_allow_html=True)

# ============================================================
# DATA LOADING
# ============================================================

@st.cache_data
def load_data():
    master_paths = [
        "master_scored.csv", 
        "data/processed/master_scored.csv",
        "../data/processed/master_scored.csv"
    ]
    val_paths = [
        "validation_results.csv", 
        "data/outputs/validation_results.csv",
        "../data/outputs/validation_results.csv"
    ]
    tier_paths = [
        "validation_by_tier.csv", 
        "data/outputs/validation_by_tier.csv",
        "../data/outputs/validation_by_tier.csv"
    ]
    
    df_master = None
    for p in master_paths:
        if os.path.exists(p):
            df_master = pd.read_csv(p)
            df_master['zip_code'] = df_master['zip_code'].astype(str).str.zfill(5)
            break
            
    df_val = None
    for p in val_paths:
        if os.path.exists(p):
            df_val = pd.read_csv(p)
            df_val['zip_code'] = df_val['zip_code'].astype(str).str.zfill(5)
            break
            
    df_tiers = None
    for p in tier_paths:
        if os.path.exists(p):
            df_tiers = pd.read_csv(p)
            break

    if df_master is not None and df_val is not None:
        # Pull BOTH prices directly from the validation file to guarantee a match
        cols_to_merge = ['zip_code', 'price_2024', 'price_latest', 'pct_change_post_model', 'cagr_post_model']
        cols_to_merge = [c for c in cols_to_merge if c in df_val.columns]
        
        # Drop price_2024 from master if it exists to prevent _x and _y duplicate columns
        if 'price_2024' in df_master.columns and 'price_2024' in cols_to_merge:
            df_master = df_master.drop(columns=['price_2024'])
            
        df = df_master.merge(df_val[cols_to_merge], on='zip_code', how='left')
    else:
        df = df_master

    
    return df, df_tiers
@st.cache_resource
def load_geojson():
    url = "https://raw.githubusercontent.com/OpenDataDE/State-zip-code-GeoJSON/master/mn_minnesota_zip_codes_geo.min.json"
    try:
        r = requests.get(url, timeout=10)
        if r.status_code == 200:
            return r.json()
        return None
    except:
        return None


@st.cache_data
def get_summary_kpis(df):
    """Precompute stat-bar values once; df never changes between rerenders."""
    scored = df[df['investment_score'].notna()]
    top_zip_row = scored.loc[scored['investment_score'].idxmax()]
    high_opp_n  = int((df['score_tier'] == 'High Opportunity').sum())
    return top_zip_row, high_opp_n


@st.cache_data
def format_validation_table(df_tiers):
    """Format the tier table once; source data never changes."""
    tier_disp = df_tiers.copy()
    tier_disp.columns = ['Tier', 'ZIPs', 'Avg Score', 'Avg Return',
                         'Median Return', 'Best ZIP', 'Worst ZIP', 'Positive %']
    tier_disp['Avg Return']    = tier_disp['Avg Return'].apply(lambda x: f"{x:.2f}%")
    tier_disp['Median Return'] = tier_disp['Median Return'].apply(lambda x: f"{x:.2f}%")
    return tier_disp

# ============================================================
# HELPERS & FINANCIALS
# ============================================================

def get_tier_class(tier):
    return {
        "High Opportunity":     "tier-high",
        "Moderate Opportunity": "tier-moderate",
        "Watch List":           "tier-watch",
        "Lower Priority":       "tier-low"
    }.get(tier, "tier-na")

def format_currency(value):
    try:
        if value is None or pd.isna(value): return "N/A"
        return f"${float(value):,.0f}"
    except: return "N/A"

def format_pct(value, decimals=1, show_sign=True):
    try:
        if value is None or pd.isna(value): return "N/A"
        sign = "+" if float(value) > 0 and show_sign else ""
        return f"{sign}{float(value):.{decimals}f}%"
    except: return "N/A"

def calculate_financials(price, monthly_rent_per_unit, rate, term, down_pct, 
                         prop_tax_rate, insurance_rate, maint_rate, vacancy_rate, 
                         mgmt_rate, num_units):
    try:
        price = float(price)
        mrent = float(monthly_rent_per_unit)
    except: return {}

    if pd.isna(price) or pd.isna(mrent) or price <= 0 or mrent <= 0:
        return {}

    down_payment = price * down_pct
    loan_amount  = price * (1 - down_pct)
    monthly_rate = rate / 12
    n_payments   = term * 12

    if monthly_rate > 0:
        monthly_mortgage = (monthly_rate * loan_amount) / (1 - (1 + monthly_rate) ** -n_payments)
    else:
        monthly_mortgage = loan_amount / n_payments

    annual_debt = monthly_mortgage * 12

    gross_monthly_rent = mrent * num_units
    gross_annual_rent  = gross_monthly_rent * 12
    vacancy_loss       = gross_annual_rent * vacancy_rate
    eff_gross_income   = gross_annual_rent - vacancy_loss

    prop_tax    = price * prop_tax_rate
    insurance   = price * insurance_rate
    maintenance = price * maint_rate
    mgmt_fee    = gross_annual_rent * mgmt_rate
    total_opex  = prop_tax + insurance + maintenance + mgmt_fee

    noi_annual  = eff_gross_income - total_opex
    noi_monthly = noi_annual / 12

    annual_cash_flow  = noi_annual - annual_debt
    monthly_cash_flow = annual_cash_flow / 12

    cash_on_cash = (annual_cash_flow / down_payment) * 100 if down_payment > 0 else 0
    cap_rate     = (noi_annual / price) * 100 if price > 0 else 0
    gross_yield  = (gross_annual_rent / price) * 100 if price > 0 else 0

    return {
        "gross_annual_rent": gross_annual_rent, "total_opex": total_opex,
        "noi_monthly": noi_monthly, "monthly_mortgage": monthly_mortgage,
        "monthly_cash_flow": monthly_cash_flow, "cash_on_cash": cash_on_cash,
        "cap_rate": cap_rate, "gross_yield": gross_yield, "down_payment": down_payment
    }

# ============================================================
# CLAUDE AI INTEGRATION
# ============================================================
def get_claude_analysis(zip_code, neighborhoods, investment_score, score_tier, 
                        price_2024, price_latest, pct_change_post_model, 
                        income_growth, pct_renter, permit_growth, monthly_rent):

    api_key = os.getenv("ANTHROPIC_API_KEY")
    if not api_key: return "⚠️ API key not configured. Add ANTHROPIC_API_KEY to your .env file."

    client = anthropic.Anthropic(api_key=api_key)
    score_str  = f"{float(investment_score):.1f}/100" if investment_score else "N/A"
    nbhd_label = (neighborhoods.split(' / ')[0] if isinstance(neighborhoods, str) else f"ZIP {zip_code}")

    prompt = f"""You are a senior real estate market analyst advising an investor on a validated scoring model.

SELECTED NEIGHBORHOOD: {nbhd_label} (ZIP {zip_code})
SCORE: {score_str} — Tier: {score_tier}

DATA:
- 2024 Price: {format_currency(price_2024)}
- 2026 Price: {format_currency(price_latest)}
- Realized Appreciation (24-26): {format_pct(pct_change_post_model)}
- Income growth (2018→2023): {format_pct(income_growth)}
- Renter share: {format_pct(pct_renter, 0, show_sign=False)}
- Est. Monthly Rent: {format_currency(monthly_rent)}

TASK — write exactly 3 sentences:
1. Explain how the recent (2024-2026) appreciation aligns with or defies the initial model score for this specific Minneapolis market.
2. Identify which demographic fundamental (renter share, income, or permits) is currently driving this trend. Also bring up whether rent share has increased or decreased and mention that relevance due to lower rent shares being the biggest predictor of greater price appreciation.
3. Suggest one concrete risk factor not found in this data that the investor must independently verify.

RULES: No headers. Separate sentences with a blank line. Max 120 words."""

    try:
        msg = client.messages.create(
            model="claude-sonnet-4-6", max_tokens=300, temperature=0.7,
            messages=[{"role": "user", "content": prompt}]
        )
        return msg.content[0].text.strip()
    except Exception as e:
        return f"⚠️ Error fetching AI insight: {str(e)}"

# ============================================================
# CHOROPLETH MAP
# ============================================================

# Columns the map hover text actually needs — used to slim the df before hashing
_MAP_COLS = ['zip_code', 'investment_score', 'price_2024', 'current_price',
             'price_latest', 'pct_change_post_model', 'pct_renter_2023']


@st.cache_data(show_spinner=False)
def create_choropleth_map(df, selected_zip=None):
    geojson = load_geojson()          # already a @st.cache_resource singleton — no copy
    if not geojson: return None
    scored_df = df[df['investment_score'].notna()].copy()

    def build_hover(row):
        return (f"<b>ZIP {row['zip_code']}</b><br>"
                f"<b>Score: {row['investment_score']:.1f}/100</b><br>"
                f"2024 Price: {format_currency(row.get('price_2024') or row.get('current_price'))}<br>"
                f"2026 Price: {format_currency(row.get('price_latest'))}<br>"
                f"24-26 Change: {format_pct(row.get('pct_change_post_model'))}<br>"
                f"Renter Share: {format_pct(row.get('pct_renter_2023'), 0, show_sign=False)}")

    scored_df['hover_text'] = scored_df.apply(build_hover, axis=1)
    
    zip_key = list(geojson['features'][0]['properties'].keys())[0]
    for k in ['ZCTA5CE10', 'GEOID10', 'ZCTA5', 'zip_code']:
        if k in geojson['features'][0]['properties']: zip_key = k; break

    fig = go.Figure(go.Choroplethmapbox(
        geojson=geojson, locations=scored_df['zip_code'], z=scored_df['investment_score'],
        featureidkey=f"properties.{zip_key}", colorscale=[[0,'#E74C3C'], [0.3,'#F39C12'], [0.6,'#F1C40F'], [1,'#27AE60']],
        zmin=20, zmax=70, marker_opacity=0.65, marker_line_width=1.5, marker_line_color="white",
        text=scored_df['hover_text'], hovertemplate="%{text}<extra></extra>",
        colorbar=dict(title="Score", tickvals=[20,35,50,65,70], len=0.6, thickness=15, bgcolor="rgba(255,255,255,0.8)")
    ))

    if selected_zip and selected_zip != "All":
        fig.add_trace(go.Choroplethmapbox(
            geojson=geojson, locations=[selected_zip], z=[1], featureidkey=f"properties.{zip_key}",
            colorscale=[[0,'rgba(0,0,0,0)'], [1,'rgba(0,0,0,0)']], showscale=False,
            marker_opacity=0, marker_line_color='#000000', marker_line_width=4, hoverinfo='skip'
        ))

    fig.update_layout(mapbox=dict(style="carto-positron", center=dict(lat=44.97, lon=-93.27), zoom=10.5),
                      margin={"r":0,"t":0,"l":0,"b":0}, height=480, paper_bgcolor="rgba(0,0,0,0)")
    return fig

@st.cache_data(show_spinner=False)
def _create_scatter_chart(scored_display, scatter_x):
    fig = px.scatter(
        scored_display, x=scatter_x,
        y='investment_score', color='score_tier', text='zip_code',
        color_discrete_map={'High Opportunity': '#27AE60', 'Moderate Opportunity': '#F39C12',
                            'Watch List': '#E67E22', 'Lower Priority': '#E74C3C'},
        labels={scatter_x: 'Median Price ($)', 'investment_score': 'Score'},
        height=350, trendline="ols"
    )
    fig.update_traces(textposition='top center')
    fig.update_layout(xaxis=dict(tickformat='$,.0f'),
                      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="#F8F9FA")
    return fig


# ============================================================
# MAIN APP
# ============================================================
def main():
    df, df_val_tiers = load_data()

    if df is None:
        st.error("Failed to load data. Make sure master_scored.csv is available.")
        return

    st.markdown("""
    <div class="main-header">
        <div>
            <h1>Minneapolis Neighborhood Investment Model</h1>
            <p style="margin-left:0.5rem;"> Data-driven scoring optimized for forward price appreciation · Zillow + Census + Permits · Built by Owen Peterson</p>
        </div>
        <a href="https://www.linkedin.com/in/owen-peterson-" target="_blank"
           style="display:inline-flex; align-items:center; gap:6px; background:rgba(255,255,255,0.1);
                  border:0.5px solid rgba(255,255,255,0.25); border-radius:8px; padding:6px 12px;
                  color:#fff; text-decoration:none; font-size:0.72rem; font-weight:500;
                  white-space:nowrap; flex-shrink:0;">
            <svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" viewBox="0 0 24 24" fill="#fff">
                <path d="M20.447 20.452h-3.554v-5.569c0-1.328-.027-3.037-1.852-3.037-1.853 0-2.136 1.445-2.136 2.939v5.667H9.351V9h3.414v1.561h.046c.477-.9 1.637-1.85 3.37-1.85 3.601 0 4.267 2.37 4.267 5.455v6.286zM5.337 7.433a2.062 2.062 0 0 1-2.063-2.065 2.064 2.064 0 1 1 2.063 2.065zm1.782 13.019H3.555V9h3.564v11.452zM22.225 0H1.771C.792 0 0 .774 0 1.729v20.542C0 23.227.792 24 1.771 24h20.451C23.2 24 24 23.227 24 22.271V1.729C24 .774 23.2 0 22.222 0h.003z"/>
            </svg>
            LinkedIn
        </a>
    </div>
    """, unsafe_allow_html=True)

    # ============================================================
    # SIDEBAR
    # ============================================================
    with st.sidebar:
        st.markdown("### 🔍 Filter & Select")
        show_city_only = st.toggle("Minneapolis City Only", value=True)
        display_df = df[df['is_mpls_city'] == True].copy() if show_city_only else df.copy()

        all_tiers = ["All"] + sorted(df['score_tier'].dropna().unique().tolist())
        selected_tier = st.selectbox("Score Tier", all_tiers)
        if selected_tier != "All": display_df = display_df[display_df['score_tier'] == selected_tier]

        # Bulletproof price column check for the filter slider
        if 'price_latest' in display_df.columns:
            price_col_filter = 'price_latest'
        elif 'price_2024' in display_df.columns:
            price_col_filter = 'price_2024'
        else:
            price_col_filter = 'current_price'

        max_price = st.slider("Max Purchase Price", 100_000, 1_500_000, 600_000, 25_000, format="$%d")
        display_df = display_df[display_df[price_col_filter].fillna(0) <= max_price]

        scored_display = display_df[display_df['investment_score'].notna()].sort_values('rank')
        zip_options = scored_display.apply(lambda r: f"{r['zip_code']} — {str(r.get('neighborhoods','')).split(' / ')[0][:30]}", axis=1).tolist()
        
        if not zip_options:
            st.warning("No neighborhoods match filters.")
            return

        selected_idx = st.selectbox("📍 Select Neighborhood", range(len(zip_options)), format_func=lambda i: zip_options[i])
        selected_row = scored_display.iloc[selected_idx]
        selected_zip = selected_row['zip_code']

        st.markdown("---")
        st.markdown("### ⚙️ Financial Assumptions")

        num_units = st.selectbox("Rentable Units", [1, 2, 3, 4], index=0)
        mortgage_rate = st.slider("Mortgage Rate (%)", 5.0, 10.0, 7.25, 0.125)
        down_pct = st.slider("Down Payment (%)", 5, 30, 20, 5) / 100
        
        default_rent = float(selected_row.get('monthly_rent_final', 1500) or 1500)
        analysis_rent = st.number_input("Est. Rent per Unit ($/mo)", value=int(default_rent), step=50)

        # Advanced Expenses
        with st.expander("Advanced Expenses & Fees", expanded=False):
            prop_tax_rate = st.number_input("Property Tax (%)", value=1.2, step=0.1) / 100
            insurance_rate = st.number_input("Insurance (%)", value=0.5, step=0.1) / 100
            maint_rate = st.number_input("Maintenance (%)", value=1.0, step=0.1) / 100
            vacancy_rate = st.number_input("Vacancy (%)", value=8.0, step=1.0) / 100
            self_manage = st.toggle("Self-manage (0% fee)", value=False)
            mgmt_rate = 0.0 if self_manage else st.number_input("Management Fee (%)", value=10.0, step=1.0) / 100

        # Bulletproof default price check
        if pd.notna(selected_row.get('price_latest')):
            default_price = float(selected_row['price_latest'])
        elif pd.notna(selected_row.get('price_2024')):
            default_price = float(selected_row['price_2024'])
        else:
            default_price = float(selected_row.get('current_price', 300000))
            
        use_custom_price = st.toggle("Enter custom purchase price", value=False)
        analysis_price = st.number_input("Purchase Price ($)", min_value=50000, value=int(default_price), step=5000) if use_custom_price else default_price

    # ============================================================
    # KPI STRIP
    # ============================================================
    top_zip_row, high_opp_n = get_summary_kpis(df)

    st.markdown(f"""
    <div class="kpi-strip">
        <div class="kpi-card">
            <div class="kpi-label">Top score</div>
            <div class="kpi-value">{top_zip_row['investment_score']:.1f}</div>
            <div class="kpi-sub">{top_zip_row['zip_code']}</div>
        </div>
        <div class="kpi-card">
            <div class="kpi-label">Average score</div>
            <div class="kpi-value">{df['investment_score'].mean():.1f}</div>
            <div class="kpi-sub">Out of 100</div>
        </div>
        <div class="kpi-card">
            <div class="kpi-label">High opportunity</div>
            <div class="kpi-value">{high_opp_n}</div>
            <div class="kpi-sub">ZIP Codes ≥ 65 Score</div>
        </div>
        <div class="kpi-card">
            <div class="kpi-label">Validated Alpha</div>
            <div class="kpi-value">r = 0.31</div>
            <div class="kpi-sub">Post-Model Predictive Correlation</div>
        </div>
    </div>
    """, unsafe_allow_html=True)

    # ============================================================
    # MAIN SPLIT
    # ============================================================
    left_col, right_col = st.columns([1.4, 1], gap="large")
        
    with left_col:
        st.markdown("#### Geographic Score Distribution")
        map_cols = [c for c in _MAP_COLS if c in display_df.columns]
        map_fig = create_choropleth_map(display_df[map_cols], selected_zip)
        if map_fig: st.plotly_chart(map_fig, use_container_width=True, config={'displayModeBar': False})

        st.markdown("#### Median Price vs. Investment Score")
        # Ensure we grab the right x-axis value safely
        if 'price_latest' in scored_display.columns:
            scatter_x = 'price_latest'
        elif 'price_2024' in scored_display.columns:
            scatter_x = 'price_2024'
        else:
            scatter_x = 'current_price'

        scatter_fig = _create_scatter_chart(scored_display, scatter_x)
        st.plotly_chart(scatter_fig, use_container_width=True, config={'displayModeBar': False})

    with right_col:
        tier_class = get_tier_class(selected_row.get('score_tier'))
        st.markdown(f"### ZIP {selected_zip}")
        st.markdown(f"<span class='tier-badge {tier_class}'>{selected_row.get('score_tier')}</span>", unsafe_allow_html=True)
        
        score_pct = max(0, min(100, (selected_row['investment_score'] - 20) / 50 * 100))
        st.markdown(f"""
        <div class="score-wrap">
            <div class="score-row"><span class="score-lbl">Investment score</span><span class="score-num">{selected_row['investment_score']:.1f} / 100</span></div>
            <div class="score-track"><div class="score-fill" style="width:{score_pct}%;"></div></div>
        </div>
        """, unsafe_allow_html=True)

        st.markdown("**📊 Market Fundamentals**")
        
        # Safely extract the prices, handling Pandas NaN values explicitly
        p_24 = selected_row.get('price_2024')
        if pd.isna(p_24):
            p_24 = selected_row.get('current_price')
            
        p_latest = selected_row.get('price_latest')

        col_a, col_b = st.columns(2)
        with col_a:
            st.markdown(f"""
            <div class="metric-card">
                <div class="metric-label">2024 Price Baseline</div>
                <div class="metric-value">{format_currency(p_24)}</div>
            </div>
            <div class="metric-card">
                <div class="metric-label">Renter Share</div>
                <div class="metric-value">{format_pct(selected_row.get('pct_renter_2023'), 0, show_sign=False)}</div>
            </div>
            """, unsafe_allow_html=True)
        with col_b:
            st.markdown(f"""
            <div class="metric-card">
                <div class="metric-label">2026 Price (Latest)</div>
                <div class="metric-value" style="color:#27AE60;">{format_currency(p_latest)}</div>
            </div>
            <div class="metric-card">
                <div class="metric-label">Post-Model Apprec.</div>
                <div class="metric-value">{format_pct(selected_row.get('pct_change_post_model'))}</div>
            </div>
            """, unsafe_allow_html=True)

        @st.fragment
        def render_financials():
            st.markdown("**💵 Return Estimates**")
            
            # The calculation runs INSIDE the fragment now
            fin = calculate_financials(
                analysis_price, analysis_rent, mortgage_rate/100, 30, down_pct, 
                prop_tax_rate, insurance_rate, maint_rate, vacancy_rate, mgmt_rate, num_units
            )
            
            if fin:
                col_c, col_d = st.columns(2)
                with col_c:
                    st.markdown(f"<div class='metric-card'><div class='metric-label'>Monthly NOI</div><div class='metric-value' style='color:#27AE60;'>{format_currency(fin['noi_monthly'])}</div></div>", unsafe_allow_html=True)
                    st.markdown(f"<div class='metric-card'><div class='metric-label'>Cap Rate</div><div class='metric-value'>{fin['cap_rate']:.2f}%</div></div>", unsafe_allow_html=True)
                with col_d:
                    st.markdown(f"<div class='metric-card'><div class='metric-label'>Monthly Cash Flow</div><div class='metric-value' style='color:{'#27AE60' if fin['monthly_cash_flow']>0 else '#E74C3C'};'>{format_currency(fin['monthly_cash_flow'])}</div></div>", unsafe_allow_html=True)
                    st.markdown(f"<div class='metric-card'><div class='metric-label'>Cash on Cash</div><div class='metric-value' style='color:{'#27AE60' if fin['cash_on_cash']>0 else '#E74C3C'};'>{fin['cash_on_cash']:.2f}%</div></div>", unsafe_allow_html=True)

        # Call the fragment
        render_financials()
        if st.button("🤖 Generate AI Analysis"):
            with st.spinner("Analyzing..."):
                analysis = get_claude_analysis(selected_zip, selected_row.get('neighborhoods'), selected_row['investment_score'], selected_row.get('score_tier'), 
                                               selected_row.get('price_2024'), selected_row.get('price_latest'), selected_row.get('pct_change_post_model'),
                                               selected_row.get('income_growth_pct'), selected_row.get('pct_renter_2023'), selected_row.get('permit_growth_pct'), analysis_rent)
                st.markdown(f"<div class='ai-box'><div class='ai-box-header'>Insight</div><div class='ai-box-text'>{analysis}</div></div>", unsafe_allow_html=True)

    # ============================================================
    # VALIDATION SECTION
    # ============================================================
    st.markdown("---")
    st.markdown("### 📈 Out-of-Sample Validation (March 2024 – March 2026)")
    st.markdown("""
    <div style="font-size:0.9rem; color:#4B5563; margin-bottom: 15px;">
    To prevent "look-ahead bias", the model was trained strictly on data available up to Q1 2024. 
    The performance below represents a <strong>blind, 2-year forward test</strong> comparing the original 2024 scores against actual 2026 home prices.
    The results confirm the model correctly ranked future winners, generating statistically significant alpha (p=0.0024).
    </div>
    """, unsafe_allow_html=True)

    if df_val_tiers is not None:
        st.dataframe(format_validation_table(df_val_tiers), use_container_width=True, hide_index=True)
    else:
        st.info("Validation Tier data (validation_by_tier.csv) not found. Run 04_validation.R to generate.")

if __name__ == "__main__":
    main()